package io.iteratee.internal

import algebra.Monoid
import cats.{ Applicative, Functor, Monad, MonoidK }
import cats.data.{ NonEmptyVector, OneAnd }
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
    ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
    ifDone: (A, Vector[E]) => Z,
    ifEnd: A => Z
  ): Z

  def isDone: Boolean

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B): Step[F, E, B]

  def contramap[E2](f: E2 => E): Step[F, E2, A]

  def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]]

  def feedEl(e: E): F[Step[F, E, A]]
  def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]]
  def end: F[Step.Ended[F, E, A]]
  def run: F[A]
}


abstract class ContStep[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] { self =>
  final def fold[Z](
    ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
    ifDone: (A, Vector[E]) => Z,
    ifEnd: A => Z
  ): Z = ifCont {
    case OneAnd(e, Vector()) => feedEl(e)
    case OneAnd(h1, h2 +: t) => feedChunk(h1, h2, t)
  }

  private[iteratee] final def unsafeValue: A = diverge[A]
  final def isDone: Boolean = false
  final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] =
    new FuncContStep[G, E, A] {
      final def feedEl(e: E): G[Step[G, E, A]] = f(F.map(self.feedEl(e))(_.mapI(f)))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): G[Step[G, E, A]] = f(F.map(self.feedChunk(h1, h2, t))(_.mapI(f)))
      final def end: G[Step.Ended[G, E, A]] =
        f(F.map(self.end)(_.mapI(f))).asInstanceOf[G[Step.Ended[G, E, A]]]
    }

  final def run: F[A] = F.map(end)(_.value)
}

abstract class FuncContStep[F[_], E, A](implicit F: Applicative[F]) extends ContStep[F, E, A] { self =>
  final def map[B](f: A => B): Step[F, E, B] = new FuncContStep[F, E, B] {
    final def feedEl(e: E): F[Step[F, E, B]] = F.map(self.feedEl(e))(_.map(f))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = F.map(self.feedChunk(h1, h2, t))(_.map(f))
    final def end: F[Step.Ended[F, E, B]] =
      F.map(self.end)(_.map(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
  }
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = new FuncContStep[F, E2, A] {
    final def feedEl(e: E2): F[Step[F, E2, A]] = F.map(self.feedEl(f(e)))(_.contramap(f))
    final def feedChunk(h1: E2, h2: E2, t: Vector[E2]): F[Step[F, E2, A]] =
      F.map(self.feedChunk(f(h1), f(h2), t.map(f)))(_.contramap(f))
    final def end: F[Step.Ended[F, E2, A]] =
      F.map(self.end)(_.contramap(f)).asInstanceOf[F[Step.Ended[F, E2, A]]]
  }
  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
    new FuncContStep[F, E, B] {
      final def feedEl(e: E): F[Step[F, E, B]] = M.flatMap(self.feedEl(e))(_.bind(f))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] =
        M.flatMap(self.feedChunk(h1, h2, t))(_.bind(f))
      final def end: F[Step.Ended[F, E, B]] =
        M.flatMap(self.end)(_.bind(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
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
  sealed abstract class Done[F[_], E, A](final val unsafeValue: A)(implicit F: Applicative[F]) extends Step[F, E, A] {
    final def isDone: Boolean = true
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(this)
    final def end: F[Ended[F, E, A]] = F.pure(new Ended(unsafeValue))
    final def run: F[A] = F.map(end)(_.value)
  }

  object Done {
    class IsDone[F[_], E, A](val step: Step[F, E, A]) extends AnyVal {
      def isEmpty: Boolean = !step.isDone
      def get: A = step.unsafeValue
    }

    final def unapply[F[_], E, A](step: Step[F, E, A]): IsDone[F, E, A] = new IsDone(step)
  }

  case class NoLeftovers[F[_]: Applicative, E, A](value: A) extends Done[F, E, A](value) {
    final def fold[Z](
      ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
      ifDone: (A, Vector[E]) => Z,
      ifEnd: A => Z
    ): Z = ifDone(value, Vector.empty)

    final def map[B](f: A => B): Step[F, E, B] = new NoLeftovers(f(value))
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new NoLeftovers(value)
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = f(value)
    final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = new NoLeftovers(value)
  }

  case class WithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Input[E]) extends Done[F, E, A](value) {
    final def fold[Z](
      ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
      ifDone: (A, Vector[E]) => Z,
      ifEnd: A => Z
    ): Z = ifDone(value, remaining.toVector)

    final def map[B](f: A => B): Step[F, E, B] = new WithLeftovers(f(value), remaining)
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new NoLeftovers(value)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      M.flatMap(f(value)) {
        case Done(v) => M.pure(new WithLeftovers(v, remaining))
        case step => remaining.foldWith(
          new Input.Folder[E, F[Step[F, E, B]]] {
            def onEl(e: E): F[Step[F, E, B]] = step.feedEl(e)
            def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = step.feedChunk(h1, h2, t)
          }
        )
      }

    final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] =
      new WithLeftovers(value, remaining)
  }

  case class Ended[F[_]: Applicative, E, A](value: A) extends Done[F, E, A](value) {
    final def fold[Z](
      ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
      ifDone: (A, Vector[E]) => Z,
      ifEnd: A => Z
    ): Z = ifEnd(value)

    final def map[B](f: A => B): Step[F, E, B] = new Ended(f(value))
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new Ended(value)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      M.flatMap(f(value))(step =>
        if (step.isDone) M.pure(new Ended(step.unsafeValue)) else step.end.asInstanceOf[F[Step[F, E, B]]]
      )

    final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = new Ended(value)
  }


  abstract class Cont[F[_]: Applicative, E, A] extends FuncContStep[F, E, A] with Input.Folder[E, F[Step[F, E, A]]] {
    final def feedEl(e: E): F[Step[F, E, A]] = onEl(e)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onChunk(h1, h2, t)
  }

  abstract class PureCont[F[_], E, A](implicit F: Applicative[F])
    extends ContStep[F, E, A] with Input.Folder[E, Step[F, E, A]]  { self =>
    final def pureApply(in: Input[E]): Step[F, E, A] = in.foldWith(this)
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(onEl(e))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(onChunk(h1, h2, t))

    final def map[B](f: A => B): Step[F, E, B] = new PureCont[F, E, B] {
      final def onEl(e: E): Step[F, E, B] = self.onEl(e).map(f)
      final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, B] = self.onChunk(h1, h2, t).map(f)
      final def end: F[Step.Ended[F, E, B]] =
        F.map(self.end)(_.map(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
    }
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new PureCont[F, E2, A] {
      final def onEl(e: E2): Step[F, E2, A] = self.onEl(f(e)).contramap(f)
      final def onChunk(h1: E2, h2: E2, t: Vector[E2]): Step[F, E2, A] =
        self.onChunk(f(h1), f(h2), t.map(f)).contramap(f)
      final def end: F[Step.Ended[F, E2, A]] =
        F.map(self.end)(_.contramap(f)).asInstanceOf[F[Step.Ended[F, E2, A]]]
    }
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new FuncContStep[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = self.onEl(e).bind(f)
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = self.onChunk(h1, h2, t).bind(f)
        final def end: F[Step.Ended[F, E, B]] =
          M.flatMap(self.end)(_.bind(f)).asInstanceOf[F[Step.Ended[F, E, B]]]
      }
    )
  }

  /**
   * Create an incomplete state that will use the given function to process the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](
    ifInput: NonEmptyVector[E] => F[Step[F, E, A]],
    ifEnd: => F[A]
  )(implicit F: Applicative[F]): Step[F, E, A] =
    new FuncContStep[F, E, A] {
      final def end: F[Ended[F, E, A]] = F.map(ifEnd)(ended(_))
      final def feedEl(e: E): F[Step[F, E, A]] = ifInput(NonEmptyVector(e))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = ifInput(NonEmptyVector(h1, h2 +: t))
    }

  /**
   * Create a new completed state with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A, remaining: Vector[E] = Vector.empty): Step[F, E, A] =
    remaining match {
      case Vector() => new NoLeftovers(value)
      case Vector(e) => new WithLeftovers(value, Input.el(e))
      case h1 +: h2 +: t => new WithLeftovers(value, Input.chunk(h1, h2, t))
    }

  final def ended[F[_]: Applicative, E, A](value: A): Ended[F, E, A] = new Ended(value)

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
      if (s.isDone) F.pure(Step.done(s.unsafeValue)) else F.flatMap(s.end)(
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

  final class FoldCont[F[_], E, A](acc: A, f: (A, E) => A)(implicit F: Applicative[F]) extends PureCont[F, E, A] {
    final def end: F[Ended[F, E, A]] = F.pure(new Ended(acc))
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
    final def end: F[Ended[F, E, A]] = F.pure(new Ended(acc))
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

  final class DrainCont[F[_], E](acc: Vector[E])(implicit F: Applicative[F]) extends PureCont[F, E, Vector[E]] {
    final def end: F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
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
    final def end: F[Ended[F, E, C[E]]] = F.pure(new Ended(acc))
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

  final class HeadCont[F[_], E](implicit F: Applicative[F]) extends PureCont[F, E, Option[E]] {
    final def end: F[Ended[F, E, Option[E]]] = F.pure(new Ended(None))
    final def onEl(e: E): Step[F, E, Option[E]] = done(Some(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.fromPair(h2, t))
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Step[F, E, Option[E]] = new PeekCont

  final class PeekCont[F[_], E](implicit F: Applicative[F]) extends PureCont[F, E, Option[E]] {
    final def end: F[Ended[F, E, Option[E]]] = F.pure(new Ended(None))
    final def onEl(e: E): Step[F, E, Option[E]] = new WithLeftovers(Some(e), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.chunk(h1, h2, t))
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

  final class TakeCont[F[_], E](acc: Vector[E], n: Int)(implicit F: Applicative[F]) extends PureCont[F, E, Vector[E]] {
    final def end: F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, Vector[E]] = if (n == 1) done(acc :+ e) else new TakeCont(acc :+ e, n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val v = h1 +: h2 +: t
      val diff = n - v.size

      if (diff > 0) new TakeCont(acc ++ v, diff) else if (diff == 0) done(acc ++ v) else {
        val (taken, left) = v.splitAt(n)

        new WithLeftovers(acc ++ taken, Input.fromVectorUnsafe(left))
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

  final class TakeWhileCont[F[_], E](acc: Vector[E], p: E => Boolean)(implicit F: Applicative[F])
    extends PureCont[F, E, Vector[E]] {
    final def end: F[Ended[F, E, Vector[E]]] = F.pure(new Ended(acc))
    final def onEl(e: E): Step[F, E, Vector[E]] =
      if (p(e)) new TakeWhileCont(acc :+ e, p) else new WithLeftovers(acc, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val (before, after) = (h1 +: h2 +: t).span(p)

      if (after.isEmpty) new TakeWhileCont(acc ++ before, p) else
        new WithLeftovers(acc ++ before, Input.fromVectorUnsafe(after))
    }
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Step[F, E, Unit] =
    if (n <= 0) done(()) else new DropCont(n)

  final class DropCont[F[_], E](n: Int)(implicit F: Applicative[F]) extends PureCont[F, E, Unit] {
    final def end: F[Ended[F, E, Unit]] = F.pure(new Ended(()))
    final def onEl(e: E): Step[F, E, Unit] = drop(n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val len = t.size + 2

      if (len <= n) drop(n - len) else {
        val dropped = (h1 +: h2 +: t).drop(n)

        new WithLeftovers((), Input.fromVectorUnsafe(dropped))
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

  final class DropWhileCont[F[_], E](p: E => Boolean)(implicit F: Applicative[F]) extends PureCont[F, E, Unit] {
    final def end: F[Ended[F, E, Unit]] = F.pure(new Ended(()))
    final def onEl(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else new WithLeftovers((), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val after = (h1 +: h2 +: t).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else new WithLeftovers((), Input.fromVectorUnsafe(after))
    }
  }

  final def isEnd[F[_], E](implicit F: Applicative[F]): Step[F, E, Boolean] = new PureCont[F, E, Boolean] {
    final def end: F[Ended[F, E, Boolean]] = F.pure(new Ended(true))
    final def onEl(e: E): Step[F, E, Boolean] = new WithLeftovers(false, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Boolean] =
      new WithLeftovers(false, Input.chunk(h1, h2, t))
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
        ifCont = k => (None, s),
        ifDone = (value, remainder) =>
          if (remainder.isEmpty) (Some((value, None)), done(value)) else {
            val input = Input.fromVectorUnsafe(remainder)

            (Some((value, Some(input))), new WithLeftovers(value, input))
          },
         ifEnd = value => (Some((value, None)), ended(value))
      )
    )

    def shorter(a: Option[Input[E]], b: Option[Input[E]]): Option[Input[E]] =
      (a, b) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(as), Some(bs)) => if (as.size <= bs.size) a else b
      }

    def endSteps(stepA: Step[F, E, A], stepB: Step[F, E, B]): F[Ended[F, E, (A, B)]] =
      F.flatMap(stepA.end)(_.bind(a => F.map(stepB.end)(_.map((a, _))))).asInstanceOf[F[Ended[F, E, (A, B)]]]

    def loop(stepA: Step[F, E, A], stepB: Step[F, E, B]): Step[F, E, (A, B)] =
      new FuncContStep[F, E, (A, B)] {
        final def end: F[Ended[F, E, (A, B)]] = endSteps(stepA, stepB)
        final def feedEl(e: E): F[Step[F, E, (A, B)]] = F.flatMap(stepA.feedEl(e))(fsA =>
          paired(fsA).bind {
            case (pairA, nextA) =>
              F.flatMap(stepB.feedEl(e))(fsB =>
                paired(fsB).bind {
                  case (pairB, nextB) => F.pure(
                    (pairA, pairB) match {
                      case (Some((resA, remA)), Some((resB, remB))) =>
                        shorter(remA, remB) match {
                          case None => new NoLeftovers[F, E, (A, B)]((resA, resB))
                          case Some(rem) => new WithLeftovers[F, E, (A, B)]((resA, resB), rem)
                        }
                      case (Some((resA, _)), None) => nextB.map((resA, _))
                      case (None, Some((resB, _))) => nextA.map((_, resB))
                      case _ => loop(nextA, nextB)
                    }
                  )
                }
              )
          }
        )
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, (A, B)]] =
          F.flatMap(stepA.feedChunk(h1, h2, t))(fsA =>
          paired(fsA).bind {
            case (pairA, nextA) =>
              F.flatMap(stepB.feedChunk(h1, h2, t))(fsB =>
                paired(fsB).bind {
                  case (pairB, nextB) => F.pure(
                    (pairA, pairB) match {
                      case (Some((resA, remA)), Some((resB, remB))) =>
                        shorter(remA, remB) match {
                          case None => new NoLeftovers[F, E, (A, B)]((resA, resB))
                          case Some(rem) => new WithLeftovers[F, E, (A, B)]((resA, resB), rem)
                        }
                      case (Some((resA, _)), None) => nextB.map((resA, _))
                      case (None, Some((resB, _))) => nextA.map((_, resB))
                      case _ => loop(nextA, nextB)
                    }
                  )
                }
              )
          }
        )
      }

    paired(stepA).bind {
      case (pairA, nextA) =>
        paired(stepB).bind {
          case (pairB, nextB) =>
            F.pure[Step[F, E, (A, B)]](
              (pairA, pairB) match {
                case (Some((resA, remA)), Some((resB, remB))) =>
                  shorter(remA, remB) match {
                    case None => new NoLeftovers[F, E, (A, B)]((resA, resB))
                    case Some(rem) => new WithLeftovers[F, E, (A, B)]((resA, resB), rem)
                  }
                case (Some((resA, _)), None) => nextB.map((resA, _))
                case (None, Some((resB, _))) => nextA.map((_, resB))
                case _ => loop(nextA, nextB)
              }
            )
        }
    }
  }
}
