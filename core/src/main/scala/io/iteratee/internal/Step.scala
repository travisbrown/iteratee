package io.iteratee.internal

import algebra.Monoid
import cats.{ Applicative, Comonad, Functor, Id, Monad, MonoidK }
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

  /**
   * Lift this [[Iteratee]] into a different context.
   */
  final def up[G[_]](implicit G: Applicative[G], F: Comonad[F]): Step[G, E, A] = mapI(
    new NaturalTransformation[F, G] {
      final def apply[A](a: F[A]): G[A] = G.pure(F.extract(a))
    }
  )
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
  sealed abstract class Done[F[_], E, A](final val value: A)(implicit F: Applicative[F]) extends Step[F, E, A] {
    final def isDone: Boolean = true
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(this)
    final def end: F[Ended[F, E, A]] = F.pure(new Ended(value))
    final def run: F[A] = F.map(end)(_.value)
  }

  object Done {
    final def unapply[F[_], E, A](step: Step[F, E, A]): Option[A] =
      if (step.isDone) Some(step.asInstanceOf[Done[F, E, A]].value) else None
  }

  class NoLeftovers[F[_]: Applicative, E, A](value: A) extends Done[F, E, A](value) {
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

  class WithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Input[E]) extends Done[F, E, A](value) {
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

  class Ended[F[_]: Applicative, E, A](value: A) extends Done[F, E, A](value) {
    final def fold[Z](
      ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
      ifDone: (A, Vector[E]) => Z,
      ifEnd: A => Z
    ): Z = ifEnd(value)

    final def map[B](f: A => B): Step[F, E, B] = new Ended(f(value))
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new Ended(value)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      M.flatMap(f(value)) {
        case Done(v) => M.pure(new Ended(v))
        case step => step.end.asInstanceOf[F[Step[F, E, B]]]
      }

    final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = new Ended(value)
  }

  abstract class Cont[F[_]: Applicative, E, A] extends FuncContStep[F, E, A] with Input.Folder[E, F[Step[F, E, A]]] {
    final def feedEl(e: E): F[Step[F, E, A]] = onEl(e)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onChunk(h1, h2, t)
  }

  /**
   * Create an incomplete state that will use the given function to process the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](
    ifInput: NonEmptyVector[E] => F[Step[F, E, A]],
    ifEnd: F[A]
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
    step.bind {
      case Done(value) => F.pure(Step.done(value))
      case next => F.map(next.run)(Step.done(_))
    }

  /**
   * A [[Step]] that folds a stream using an initial value and an accumulation
   * function.
   *
   * @group Collection
   */
  final def fold[E, A](init: A)(f: (A, E) => A): Step[Id, E, A] = new FoldCont[E, A](init, f)

  final class FoldCont[E, A](acc: A, f: (A, E) => A) extends Cont[Id, E, A] {
    final def end: Ended[Id, E, A] = new Ended(acc)
    final def onEl(e: E): Step[Id, E, A] = self.fold(f(acc, e))(f)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, A] =
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
  final def drain[A]: Step[Id, A, Vector[A]] = new DrainCont(Vector.empty)

  final class DrainCont[E](acc: Vector[E]) extends Cont[Id, E, Vector[E]] {
    final def end: Ended[Id, E, Vector[E]] = new Ended(acc)
    final def onEl(e: E): Step[Id, E, Vector[E]] = new DrainCont(acc :+ e)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Vector[E]] = new DrainCont(acc ++ (h1 +: h2 +: t))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def drainTo[E, C[_]](implicit M: MonoidK[C], C: Applicative[C]): Step[Id, E, C[E]] = new DrainToCont(M.empty)

  final class DrainToCont[E, C[_]](acc: C[E])(implicit
    M: MonoidK[C],
    C: Applicative[C]
  ) extends Cont[Id, E, C[E]] {
    final def end: Ended[Id, E, C[E]] = new Ended(acc)
    final def onEl(e: E): Step[Id, E, C[E]] = new DrainToCont(M.combine(acc, C.pure(e)))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, C[E]] = new DrainToCont(
      t.foldLeft(M.combine(M.combine(acc, C.pure(h1)), C.pure(h2)))((a, e) => M.combine(a, C.pure(e)))
    )
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[E]: Step[Id, E, Option[E]] = new HeadCont

  final class HeadCont[E] extends Cont[Id, E, Option[E]] {
    final def end: Ended[Id, E, Option[E]] = new Ended(None)
    final def onEl(e: E): Step[Id, E, Option[E]] = done(Some(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.fromPair(h2, t))
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[E]: Step[Id, E, Option[E]] = new PeekCont

  final class PeekCont[E] extends Cont[Id, E, Option[E]] {
    final def end: Ended[Id, E, Option[E]] = new Ended(None)
    final def onEl(e: E): Step[Id, E, Option[E]] = new WithLeftovers(Some(e), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.chunk(h1, h2, t))
  }

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[A](n: Int): Step[Id, A, Vector[A]] = if (n <= 0) {
    Step.done[Id, A, Vector[A]](Vector.empty)
  } else {
    new TakeCont(Vector.empty, n)
  }

  final class TakeCont[E](acc: Vector[E], n: Int) extends Cont[Id, E, Vector[E]] {
    final def end: Ended[Id, E, Vector[E]] = new Ended(acc)
    final def onEl(e: E): Step[Id, E, Vector[E]] = if (n == 1) done(acc :+ e) else new TakeCont(acc :+ e, n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Vector[E]] = {
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
  final def takeWhile[E](p: E => Boolean): Step[Id, E, Vector[E]] = new TakeWhileCont(Vector.empty, p)

  final class TakeWhileCont[E](acc: Vector[E], p: E => Boolean) extends Cont[Id, E, Vector[E]] {
    final def end: Ended[Id, E, Vector[E]] = new Ended(acc)
    final def onEl(e: E): Step[Id, E, Vector[E]] =
      if (p(e)) new TakeWhileCont(acc :+ e, p) else new WithLeftovers(acc, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Vector[E]] = {
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
  final def drop[E](n: Int): Step[Id, E, Unit] = if (n <= 0) done(()) else new DropCont(n)

  final class DropCont[E](n: Int) extends Cont[Id, E, Unit] {
    final def end: Ended[Id, E, Unit] = new Ended(())
    final def onEl(e: E): Step[Id, E, Unit] = drop(n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Unit] = {
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
  final def dropWhile[E](p: E => Boolean): Step[Id, E, Unit] = new DropWhileCont(p)

  final class DropWhileCont[E](p: E => Boolean) extends Cont[Id, E, Unit] {
    final def end: Ended[Id, E, Unit] = new Ended(())
    final def onEl(e: E): Step[Id, E, Unit] = if (p(e)) dropWhile(p) else new WithLeftovers((), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Unit] = {
      val after = (h1 +: h2 +: t).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else new WithLeftovers((), Input.fromVectorUnsafe(after))
    }
  }

  final def isEnd[E]: Step[Id, E, Boolean] = new Cont[Id, E, Boolean] {
    final def end: Ended[Id, E, Boolean] = new Ended(true)
    final def onEl(e: E): Step[Id, E, Boolean] = new WithLeftovers(false, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[Id, E, Boolean] =
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
