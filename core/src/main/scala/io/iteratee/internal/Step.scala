package io.iteratee.internal

import algebra.Monoid
import cats.{ Applicative, Functor, Monad, MonoidK }
import cats.data.{ NonEmptyVector, OneAnd }
import cats.arrow.NaturalTransformation

/**
 * Represents the current state of an [[io.iteratee.Iteratee]].
 *
 * @tparam F The effect type constructor
 * @tparam E The type of the input data
 * @tparam A The type of the result calculated by the [[io.iteratee.Iteratee]]
 */
abstract class Step[F[_], E, A] extends Serializable {
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
   * Feed a single element to this [[Step]].
   */
  def feedEl(e: E): F[Step[F, E, A]]

  /**
   * Feed a multi-element [[Input]] to this [[Step]].
   */
  def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]]

  /**
   * Force this [[Step]] to enter the ended state.
   */
  def end: F[Done.Ended[F, E, A]]

  /**
   * Run this [[Step]] so that it produces a value in an effectful context.
   */
  def run: F[A]

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B): Step[F, E, B]

  /**
   * Map a function over the inputs of this [[Step]].
   */
  def contramap[E2](f: E2 => E): Step[F, E2, A]

  /**
   * Transform the context of this [[Step]].
   */
  def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]]

  /**
   * Zip this [[Step]] with another.
   */
  def zip[B](other: Step[F, E, B])(implicit M: Monad[F]): F[Step[F, E, (A, B)]]
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
  /**
   * Create an incomplete [[Step]] that will use the given functions to process
   * the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](
    onInput: NonEmptyVector[E] => F[Step[F, E, A]],
    onEnd: F[A]
  )(implicit F: Applicative[F]): Step[F, E, A] = new Cont.Effectful[F, E, A] {
    final def feedEl(e: E): F[Step[F, E, A]] = onInput(NonEmptyVector(e))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onInput(NonEmptyVector(h1, h2 +: t))
    final def end: F[Done.Ended[F, E, A]] = F.map(onEnd)(ended(_))
  }

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A, remaining: Vector[E] = Vector.empty): Step[F, E, A] =
    remaining match {
      case Vector() => new Done.NoLeftovers(value)
      case Vector(e) => new Done.WithLeftovers(value, Input.el(e))
      case h1 +: h2 +: t => new Done.WithLeftovers(value, Input.chunk(h1, h2, t))
    }

  /**
   * Create a new ended [[Step]] with the given result.
   *
   * @group Constructors
   */
  final def ended[F[_]: Applicative, E, A](value: A): Done.Ended[F, E, A] = new Done.Ended(value)

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
      case Done(value) => F.pure(done(value))
      case next => F.map(next.run)(done(_))
    }

  /**
   * A [[Step]] that folds a stream using an initial value and an accumulation
   * function.
   *
   * @group Collection
   */
  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Step[F, E, A] = new FoldCont[F, E, A](init, f)

  private[this] final class FoldCont[F[_], E, A](acc: A, f: (A, E) => A)(implicit F: Applicative[F])
    extends Cont.PureFolder[F, E, A] {
    final def onEl(e: E): Step[F, E, A] = self.fold(f(acc, e))(f)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, A] =
      self.fold(t.foldLeft(f(f(acc, h1), h2))(f))(f)
    final def end: F[Done.Ended[F, E, A]] = F.pure(new Done.Ended(acc))
  }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_]: Monad, E, A](init: A)(f: (A, E) => F[A]): Step[F, E, A] = new FoldMCont[F, E, A](init, f)

  private[this] final class FoldMCont[F[_], E, A](acc: A, f: (A, E) => F[A])(implicit F: Monad[F])
    extends Cont.EffectfulFolder[F, E, A] {
    final def end: F[Done.Ended[F, E, A]] = F.pure(new Done.Ended(acc))
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
  final def drain[F[_]: Applicative, A]: Step[F, A, Vector[A]] = new DrainCont(Vector.empty)

  private[this] final class DrainCont[F[_], E](acc: Vector[E])(implicit F: Applicative[F])
    extends Cont.PureFolder[F, E, Vector[E]] {
    final def onEl(e: E): Step[F, E, Vector[E]] = new DrainCont(acc :+ e)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = new DrainCont(acc ++ (h1 +: h2 +: t))
    final def end: F[Done.Ended[F, E, Vector[E]]] = F.pure(new Done.Ended(acc))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def drainTo[F[_], E, C[_]](implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ): Step[F, E, C[E]] = new DrainToCont(M.empty)

  private[this] final class DrainToCont[F[_], E, C[_]](acc: C[E])(implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ) extends Cont.PureFolder[F, E, C[E]] {
    final def onEl(e: E): Step[F, E, C[E]] = new DrainToCont(M.combine(acc, C.pure(e)))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, C[E]] = new DrainToCont(
      t.foldLeft(M.combine(M.combine(acc, C.pure(h1)), C.pure(h2)))((a, e) => M.combine(a, C.pure(e)))
    )
    final def end: F[Done.Ended[F, E, C[E]]] = F.pure(new Done.Ended(acc))
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_]: Applicative, E]: Step[F, E, Option[E]] = new HeadCont

  private[this] final class HeadCont[F[_], E](implicit F: Applicative[F]) extends Cont.PureFolder[F, E, Option[E]] {
    final def onEl(e: E): Step[F, E, Option[E]] = done(Some(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new Done.WithLeftovers(Some(h1), Input.fromPair(h2, t))
    final def end: F[Done.Ended[F, E, Option[E]]] = F.pure(new Done.Ended(None))
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Step[F, E, Option[E]] = new PeekCont

  private[this] final class PeekCont[F[_], E](implicit F: Applicative[F]) extends Cont.PureFolder[F, E, Option[E]] {
    final def onEl(e: E): Step[F, E, Option[E]] = new Done.WithLeftovers(Some(e), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new Done.WithLeftovers(Some(h1), Input.chunk(h1, h2, t))
    final def end: F[Done.Ended[F, E, Option[E]]] = F.pure(new Done.Ended(None))
  }

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, A](n: Int): Step[F, A, Vector[A]] = if (n <= 0) {
    done[F, A, Vector[A]](Vector.empty)
  } else {
    new TakeCont(Vector.empty, n)
  }

  private[this] final class TakeCont[F[_], E](acc: Vector[E], n: Int)(implicit F: Applicative[F])
    extends Cont.PureFolder[F, E, Vector[E]] {
    final def onEl(e: E): Step[F, E, Vector[E]] = if (n == 1) done(acc :+ e) else new TakeCont(acc :+ e, n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val v = h1 +: h2 +: t
      val diff = n - v.size

      if (diff > 0) new TakeCont(acc ++ v, diff) else if (diff == 0) done(acc ++ v) else {
        val (taken, left) = v.splitAt(n)

        new Done.WithLeftovers(acc ++ taken, Input.fromVectorUnsafe(left))
      }
    }
    final def end: F[Done.Ended[F, E, Vector[E]]] = F.pure(new Done.Ended(acc))
  }

  /**
   * A [[Step]] that returns values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Vector[E]] =
    new TakeWhileCont(Vector.empty, p)

  private[this] final class TakeWhileCont[F[_], E](acc: Vector[E], p: E => Boolean)(implicit F: Applicative[F])
    extends Cont.PureFolder[F, E, Vector[E]] {
    final def onEl(e: E): Step[F, E, Vector[E]] =
      if (p(e)) new TakeWhileCont(acc :+ e, p) else new Done.WithLeftovers(acc, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = {
      val (before, after) = (h1 +: h2 +: t).span(p)

      if (after.isEmpty) new TakeWhileCont(acc ++ before, p) else
        new Done.WithLeftovers(acc ++ before, Input.fromVectorUnsafe(after))
    }
    final def end: F[Done.Ended[F, E, Vector[E]]] = F.pure(new Done.Ended(acc))
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Step[F, E, Unit] =
    if (n <= 0) done(()) else new DropCont(n)

  private[this] final class DropCont[F[_], E](n: Int)(implicit F: Applicative[F]) extends Cont.PureFolder[F, E, Unit] {
    final def onEl(e: E): Step[F, E, Unit] = drop(n - 1)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val len = t.size + 2

      if (len <= n) drop(n - len) else {
        val dropped = (h1 +: h2 +: t).drop(n)

        new Done.WithLeftovers((), Input.fromVectorUnsafe(dropped))
      }
    }
    final def end: F[Done.Ended[F, E, Unit]] = F.pure(new Done.Ended(()))
  }

  /**
   * A [[Step]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Unit] =
    new DropWhileCont(p)

  private[this] final class DropWhileCont[F[_], E](p: E => Boolean)(implicit F: Applicative[F])
    extends Cont.PureFolder[F, E, Unit] {
    final def onEl(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else new Done.WithLeftovers((), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val after = (h1 +: h2 +: t).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else new Done.WithLeftovers((), Input.fromVectorUnsafe(after))
    }
    final def end: F[Done.Ended[F, E, Unit]] = F.pure(new Done.Ended(()))
  }

  final def isEnd[F[_], E](implicit F: Applicative[F]): Step[F, E, Boolean] = new Cont.PureFolder[F, E, Boolean] {
    final def onEl(e: E): Step[F, E, Boolean] = new Done.WithLeftovers(false, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Boolean] =
      new Done.WithLeftovers(false, Input.chunk(h1, h2, t))
    final def end: F[Done.Ended[F, E, Boolean]] = F.pure(new Done.Ended(true))
  }
}
