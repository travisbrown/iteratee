package io.iteratee.internal

import algebra.Monoid
import cats.{ Applicative, Functor, Monad, MonoidK }
import cats.arrow.NaturalTransformation
import io.iteratee.Iteratee

/**
 * Represents the current state of an [[Iteratee]].
 *
 * An [[Iteratee]] has either already calculated a result ([[Step.done]]) or is
 * waiting for more data ([[Step.cont]]).
 *
 * @tparam F The effect type constructor
 * @tparam E The type of the input data
 * @tparam A The type of the result calculated by the [[Iteratee]]
 */
sealed abstract class Step[F[_], E, A] extends Serializable {
  /**
   * The [[Iteratee]]'s result.
   *
   * In some cases we know that an iteratee has been constructed in such a way
   * that it must be in a completed state, even though that's not tracked by the
   * type system. This method provides (unsafe) access to the result for use in
   * these situations.
   */
  private[iteratee] def unsafeValue: A

  /**
   * Reduce this [[Step]] to a value using the given functions.
   */
  def fold[Z](
    onCont: (Input[E] => F[Step[F, E, A]]) => Z,
    onDone: A => Z,
    onEarly: (A, Input[E]) => Z,
    onEnd: A => Z
  ): Z

  def isDone: Boolean

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B]

  def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A]

  def mapI[G[_]](f: NaturalTransformation[F, G])(implicit F: Applicative[F]): Step[G, E, A]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]]

  /**
   * Apply this [[Step]] to an [[Input]].
   */
  def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]]

  def onEnd(implicit F: Applicative[F]): F[Step.Ended[F, E, A]]
}


abstract class ContStep[F[_], E, A] extends Step[F, E, A] with Function[Input[E], F[Step[F, E, A]]] { self =>
  final def fold[Z](
    onCont: (Input[E] => F[Step[F, E, A]]) => Z,
    onDone: A => Z,
    onEarly: (A, Input[E]) => Z,
    onEnd: A => Z
  ): Z =
    onCont(this)
  private[iteratee] final def unsafeValue: A = diverge[A]
  final def isDone: Boolean = false
  final def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = apply(in)
  final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit F: Applicative[F]): Step[G, E, A] =
    new FuncContStep[G, E, A] {
      def apply(in: Input[E]): G[Step[G, E, A]] = f(F.map(self(in))(_.mapI(f)))
      def onEnd(implicit G: Applicative[G]): G[Step.Ended[G, E, A]] =
        f(F.map(self.onEnd)(_.mapI(f))).asInstanceOf[G[Step.Ended[G, E, A]]]
    }
}

abstract class FuncContStep[F[_], E, A] extends ContStep[F, E, A] { self =>
  final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = new FuncContStep[F, E, B] {
    final def apply(in: Input[E]): F[Step[F, E, B]] = F.map(self(in))(_.map(f))
    final def onEnd(implicit F1: Applicative[F]): F[Step.Ended[F, E, B]] =
      F.map(self.onEnd(F1))(_.map(f)(F)).asInstanceOf[F[Step.Ended[F, E, B]]]
  }
  final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A] = new FuncContStep[F, E2, A] {
    final def apply(in: Input[E2]): F[Step[F, E2, A]] = F.map(self(in.map(f)))(_.contramap(f))
    final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E2, A]] =
      F.map(self.onEnd(F0))(_.contramap(f)(F)).asInstanceOf[F[Step.Ended[F, E2, A]]]
  }
  final def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
    new FuncContStep[F, E, B] {
      final def apply(in: Input[E]): F[Step[F, E, B]] = F.flatMap(self(in))(_.bind(f))
      final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E, B]] =
        F.flatMap(self.onEnd)(_.bind(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
    }
  )
}

abstract class PureFuncContStep[F[_]: Applicative, E, A] extends ContStep[F, E, A] { self =>
  def pureApply(in: Input[E]): Step[F, E, A]
  final def apply(in: Input[E]): F[Step[F, E, A]] = Applicative[F].pure(pureApply(in))
  final def map[B](f: A => B)(implicit F0: Functor[F]): Step[F, E, B] = new PureFuncContStep[F, E, B] {
    final def pureApply(in: Input[E]): Step[F, E, B] = self.pureApply(in).map(f)
    final def onEnd(implicit F1: Applicative[F]): F[Step.Ended[F, E, B]] =
      F0.map(self.onEnd(F1))(_.map(f)(F0)).asInstanceOf[F[Step.Ended[F, E, B]]]
  }
  final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A] = new PureFuncContStep[F, E2, A] {
    final def pureApply(in: Input[E2]): Step[F, E2, A] = self.pureApply(in.map(f)).contramap(f)
    final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E2, A]] =
      F.map(self.onEnd(F0))(_.contramap(f)(F)).asInstanceOf[F[Step.Ended[F, E2, A]]]
  }
  final def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
    new FuncContStep[F, E, B] {
      final def apply(in: Input[E]): F[Step[F, E, B]] = self.pureApply(in).bind(f)
      final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E, B]] =
        F.flatMap(self.onEnd)(_.bind(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
    }
  )
}

/**
 * @groupname Constructors Constructors
 * @groupprio Constructors 0
 *
 * @groupname Utilities Miscellaneous utilities
 * @groupprio Utilities 1
 *
 * @groupname Collection Collection operation steps
 * @groupprio Collection 2
 */
final object Step { self =>
  sealed abstract class Done[F[_], E, A](final val unsafeValue: A) extends Step[F, E, A] {
    final def isDone: Boolean = true
    final def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(this)
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, A]] = F.pure(new Ended(unsafeValue))
  }

  case class NoLeftovers[F[_], E, A](value: A) extends Done[F, E, A](value) {
    final def fold[Z](
      onCont: (Input[E] => F[Step[F, E, A]]) => Z,
      onDone: A => Z,
      onEarly: (A, Input[E]) => Z,
      onEnd: A => Z
    ): Z =
      onDone(value)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = new NoLeftovers(f(value))
    final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A] = new NoLeftovers(value)
    final def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = f(value)
    final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit F: Applicative[F]): Step[G, E, A] =
      new NoLeftovers(value)
  }

  case class WithLeftovers[F[_], E, A](value: A, remaining: Input[E]) extends Done[F, E, A](value) {
    final def fold[Z](
      onCont: (Input[E] => F[Step[F, E, A]]) => Z,
      onDone: A => Z,
      onEarly: (A, Input[E]) => Z,
      onEnd: A => Z
    ): Z =
        onEarly(value, remaining)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = new WithLeftovers(f(value), remaining)
    final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A] = new NoLeftovers(value)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] =
      F.flatMap(f(value))(step =>
        if (step.isDone) F.pure(new WithLeftovers(step.unsafeValue, remaining)) else step.feed(remaining)
      )

    final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit F: Applicative[F]): Step[G, E, A] =
      new WithLeftovers(value, remaining)
  }

  case class Ended[F[_], E, A](value: A) extends Done[F, E, A](value) {
    final def fold[Z](
      onCont: (Input[E] => F[Step[F, E, A]]) => Z,
      onDone: A => Z,
      onEarly: (A, Input[E]) => Z,
      onEnd: A => Z
    ): Z =
        onEnd(value)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = new Ended(f(value))
    final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Step[F, E2, A] = new Ended(value)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] =
      F.flatMap(f(value))(step =>
        if (step.isDone) F.pure(new Ended(step.unsafeValue)) else step.onEnd.asInstanceOf[F[Step[F, E, B]]]
      )

    final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit F: Applicative[F]): Step[G, E, A] =
      new Ended(value)
  }


  abstract class Cont[F[_], E, A] extends FuncContStep[F, E, A] with Input.Folder[E, F[Step[F, E, A]]] {
    final def apply(in: Input[E]): F[Step[F, E, A]] = in.foldWith(this)
    //final def onEndX: F[Step[F, E, A]] = onEnd.asInstanceOf[F[Step[F, E, A]]]
  }

  abstract class PureCont[F[_]: Applicative, E, A]
    extends PureFuncContStep[F, E, A] with Input.Folder[E, Step[F, E, A]] {
    final def pureApply(in: Input[E]): Step[F, E, A] = in.foldWith(this)
    //final def onEndX: Step[F, E, A] = onEnd
  }

  /**
   * Create an incomplete state that will use the given function to process the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](ifEnd: => F[Ended[F, E, A]], ifInput: Input[E] => F[Step[F, E, A]]): Step[F, E, A] =
    new FuncContStep[F, E, A] {
      final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, A]] = ifEnd
      final def apply(in: Input[E]): F[Step[F, E, A]] = ifInput(in)
    }

  /**
   * Create a new completed state with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_], E, A](value: A): Step[F, E, A] = new NoLeftovers(value)

  /**
   * Create a new completed state with the given result and leftover input.
   *
   * @group Constructors
   */
  final def early[F[_], E, A](value: A, remaining: Input[E]): Step[F, E, A] = new WithLeftovers(value, remaining)

  final def ended[F[_], E, A](value: A): Ended[F, E, A] = new Ended(value)

  /**
   * Lift a monadic value into a [[Step]].
   *
   * @group Utilities
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): F[Step[F, E, A]] = F.map(fa)(a => done(a))

  /**
   * Collapse a nested [[Step]] into one layer.
   *
   * @group Utilities
   */
  final def joinI[F[_], A, B, C](step: Step[F, A, Step[F, B, C]])(implicit F: Monad[F]): F[Step[F, A, C]] =
    step.bind(s =>
      if (s.isDone) F.pure(Step.done(s.unsafeValue)) else F.flatMap(s.onEnd)(
        next => if (next.isDone) F.pure(Step.done(next.unsafeValue)) else diverge
      )
    )

  /**
   * A [[Step]] that folds a stream using an initial value and an accumulation
   * function.
   *
   * @group Collection
   */
  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Step[F, E, A] =
    new FoldCont[F, E, A](init, f)

  final class FoldCont[F[_]: Applicative, E, A](acc: A, f: (A, E) => A) extends PureCont[F, E, A] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, A]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, A] = self.fold(f(acc, e))(f)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, A] =
      self.fold(t.foldLeft(f(f(acc, h1), h2))(f))(f)
  }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] =
    new FoldMCont[F, E, A](init, f)

  final class FoldMCont[F[_], E, A](acc: A, f: (A, E) => F[A])(implicit F: Monad[F]) extends Cont[F, E, A] {
    final def onEnd(implicit F0: Applicative[F]): F[Ended[F, E, A]] = F0.pure(new Ended(acc))
    final def onEl(e: E): F[Step[F, E, A]] = F.map(f(acc, e))(a => foldM(a)(f))
    final def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] =
      F.map(
        t.foldLeft(F.flatMap(f(acc, h1))(a => f(a, h2)))((fa, e) => F.flatMap(fa)(a => f(a, e)))
      )(a => foldM(a)(f))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a vector.
   *
   * @group Collection
   */
  final def drain[F[_]: Applicative, A]: Step[F, A, Vector[A]] =
    new DrainCont(Vector.empty)

  final class DrainCont[F[_]: Applicative, E](acc: Vector[E]) extends PureCont[F, E, Vector[E]] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, Vector[E]] = new DrainCont(acc :+ e)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = new DrainCont(acc ++ (h1 +: h2 +: t))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def drainTo[F[_], E, C[_]](implicit
    F: Monad[F],
    M: MonoidK[C],
    C: Applicative[C]
  ): Step[F, E, C[E]] = new DrainToCont(M.empty)

  final class DrainToCont[F[_], E, C[_]](acc: C[E])(implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ) extends PureCont[F, E, C[E]] {
    final def onEnd(implicit F0: Applicative[F]): F[Ended[F, E, C[E]]] = F0.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, C[E]] = new DrainToCont(M.combine(acc, C.pure(e)))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, C[E]] = new DrainToCont(
      t.foldLeft(M.combine(M.combine(acc, C.pure(h1)), C.pure(h2)))((a, e) => M.combine(a, C.pure(e)))
    )
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_]: Applicative, E]: Step[F, E, Option[E]] = new HeadCont

  final class HeadCont[F[_]: Applicative, E] extends PureCont[F, E, Option[E]] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Option[E]]] = F.pure(new Ended(None))
    final def onEl(e: E): Step[F, E, Option[E]] = done(Some(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] = early(Some(h1), Input.fromPair(h2, t))
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Step[F, E, Option[E]] = new PeekCont

  final class PeekCont[F[_]: Applicative, E] extends PureCont[F, E, Option[E]] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Option[E]]] = F.pure(new Ended(None))
    final def onEl(e: E): Step[F, E, Option[E]] = early(Some(e), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] = early(Some(h1), Input.chunk(h1, h2, t))
  }

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, A](n: Int): Step[F, A, Vector[A]] = if (n <= 0) {
    Step.done[F, A, Vector[A]](Vector.empty)
  } else {
    new TakeCont(Vector.empty, n)
  }

  final class TakeCont[F[_]: Applicative, E](acc: Vector[E], n: Int) extends PureCont[F, E, Vector[E]] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, Vector[E]] = if (n == 1) done(acc :+ e) else new TakeCont(acc :+ e, n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val v = h1 +: h2 +: t
      val diff = n - v.size

      if (diff > 0) new TakeCont(acc ++ v, diff) else if (diff == 0) done(acc ++ v) else {
        val (taken, left) = v.splitAt(n)

        early(acc ++ taken, Input.fromVectorUnsafe(left))
      }
    }
  }

  /**
   * A [[Step]] that returns values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Vector[E]] =
    new TakeWhileCont(Vector.empty, p)

  final class TakeWhileCont[F[_]: Applicative, E](acc: Vector[E], p: E => Boolean) extends PureCont[F, E, Vector[E]] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, Vector[E]] = if (p(e)) new TakeWhileCont(acc :+ e, p) else early(acc, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val (before, after) = (h1 +: h2 +: t).span(p)

      if (after.isEmpty) new TakeWhileCont(acc ++ before, p) else
        early(acc ++ before, Input.fromVectorUnsafe(after))
    }
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Step[F, E, Unit] =
    if (n <= 0) done(()) else new DropCont(n)

  final class DropCont[F[_]: Applicative, E](n: Int) extends PureCont[F, E, Unit] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Unit]] = F.pure(new Ended(()))
    final def onEl(e: E): Step[F, E, Unit] = drop(n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val len = t.size + 2

      if (len <= n) drop(n - len) else {
        val dropped = (h1 +: h2 +: t).drop(n)

        early((), Input.fromVectorUnsafe(dropped))
      }
    }
  }

  /**
   * A [[Step]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Unit] =
    new DropWhileCont(p)

  final class DropWhileCont[F[_]: Applicative, E](p: E => Boolean) extends PureCont[F, E, Unit] {
    final def onEnd(implicit F: Applicative[F]): F[Ended[F, E, Unit]] = F.pure(new Ended(()))
    final def onEl(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else early((), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val after = (h1 +: h2 +: t).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else early((), Input.fromVectorUnsafe(after))
    }
  }

  final def isEnd[F[_]: Applicative, E]: Step[F, E, Boolean] = new PureCont[F, E, Boolean] {
    final def onEnd(implicit F0: Applicative[F]): F[Ended[F, E, Boolean]] = F0.pure(new Ended(true))
    final def onEl(e: E): Step[F, E, Boolean] = early(false, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Boolean] = early(false, Input.chunk(h1, h2, t))
  }

  /**
   * Zip two steps into a single step that returns a pair.
   *
   * @group Collection
   */
  final def zip[F[_], E, A, B](stepA: Step[F, E, A], stepB: Step[F, E, B])
    (implicit F: Monad[F]): F[Step[F, E, (A, B)]] = {
    type Pair[Z] = (Option[(Z, Option[Input[E]])], Step[F, E, Z])

    def paired[Z](s: Step[F, E, Z]): Step[F, E, Pair[Z]] = done(
      s.fold(
         k => (None, s),
         value => (Some((value, None)), done(value)),
         (value, remainder) => (Some((value, Some(remainder))), early(value, remainder)),
         value => (Some((value, None)), ended(value))
      )
    )

    def shorter(a: Option[Input[E]], b: Option[Input[E]]): Option[Input[E]] =
      (a, b) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(as), Some(bs)) => if (as.size <= bs.size) a else b
      }

    def end(stepA: Step[F, E, A], stepB: Step[F, E, B]): F[Ended[F, E, (A, B)]] =
      F.flatMap(stepA.onEnd)(_.bind(a => F.map(stepB.onEnd)(_.map((a, _))))).asInstanceOf[F[Ended[F, E, (A, B)]]]

    def loop(stepA: Step[F, E, A], stepB: Step[F, E, B])(in: Input[E]): F[Step[F, E, (A, B)]] =
      F.flatMap(stepA.feed(in))(fsA =>
        paired(fsA).bind {
          case (pairA, nextA) =>
            F.flatMap(stepB.feed(in))(fsB =>
              paired(fsB).bind {
                case (pairB, nextB) => F.pure(
                  (pairA, pairB) match {
                    case (Some((resA, remA)), Some((resB, remB))) =>
                      shorter(remA, remB) match {
                        case None => Step.done[F, E, (A, B)]((resA, resB))
                        case Some(rem) => Step.early[F, E, (A, B)]((resA, resB), rem)
                      }
                    case (Some((resA, _)), None) => nextB.map((resA, _))
                    case (None, Some((resB, _))) => nextA.map((_, resB))
                    case _ => Step.cont(end(nextA, nextB), loop(nextA, nextB))
                  }
                )
              }
            )
        }
      )

    paired(stepA).bind {
      case (pairA, nextA) =>
        paired(stepB).bind {
          case (pairB, nextB) =>
            F.pure[Step[F, E, (A, B)]](
              (pairA, pairB) match {
                case (Some((resA, remA)), Some((resB, remB))) =>
                  shorter(remA, remB) match {
                    case None => Step.done[F, E, (A, B)]((resA, resB))
                    case Some(rem) => Step.early[F, E, (A, B)]((resA, resB), rem)
                  }
                case (Some((resA, _)), None) => nextB.map((resA, _))
                case (None, Some((resB, _))) => nextA.map((_, resB))
                case _ => Step.cont(end(nextA, nextB), loop(nextA, nextB))
              }
            )
        }
    }
  }
}
