package io.iteratee.internal

import cats.{ Applicative, Eval, Monad, Monoid, MonoidK, Semigroup }
import cats.data.NonEmptyVector
import cats.arrow.FunctionK

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
  def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z

  def isDone: Boolean

  /**
   * Run this [[Step]] so that it produces a value in an effectful context.
   */
  def run: F[A]

  /**
   * Feed a single element to this [[Step]].
   */
  def feedEl(e: E): F[Step[F, E, A]]

  /**
   * Feed a multi-element [[Input]] to this [[Step]].
   */
  def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]]

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
  def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]]

  /**
   * Zip this [[Step]] with another.
   */
  def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)]
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
  abstract class Cont[F[_]: Applicative, E, A] extends EffectfulCont[F, E, A] with Input.Folder[E, F[Step[F, E, A]]] {
    final def feedEl(e: E): F[Step[F, E, A]] = onEl(e)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onChunk(h1, h2, t)
  }

  object Cont {
    abstract class WithValue[F[_], E, A](value: A)(implicit F: Applicative[F]) extends Cont[F, E, A] {
      final def run: F[A] = F.pure(value)
    }
  }

  abstract class PureCont[F[_], E, A](implicit F: Applicative[F])
    extends BaseCont[F, E, A] with Input.Folder[E, Step[F, E, A]]  { self =>
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(onEl(e))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(onChunk(h1, h2, t))

    final def map[B](f: A => B): Step[F, E, B] = new PureCont[F, E, B] {
      final def onEl(e: E): Step[F, E, B] = self.onEl(e).map(f)
      final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, B] = self.onChunk(h1, h2, t).map(f)
      final def run: F[B] = F.map(self.run)(f)
    }
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new PureCont[F, E2, A] {
      final def onEl(e: E2): Step[F, E2, A] = self.onEl(f(e)).contramap(f)
      final def onChunk(h1: E2, h2: E2, t: Vector[E2]): Step[F, E2, A] =
        self.onChunk(f(h1), f(h2), t.map(f)).contramap(f)
      final def run: F[A] = self.run
    }
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new EffectfulCont[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = self.onEl(e).bind(f)
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = self.onChunk(h1, h2, t).bind(f)
        final def run: F[B] = M.flatMap(self.run)(a => M.flatMap(f(a))(_.run))
      }
    )
  }

  object PureCont {
    abstract class WithValue[F[_], E, A](value: A)(implicit F: Applicative[F]) extends PureCont[F, E, A] {
      final def run: F[A] = F.pure(value)
    }
  }

  final object Done {
    final def unapply[F[_], E, A](step: Step[F, E, A]): Option[A] =
      if (step.isDone) Some(step.asInstanceOf[BaseDone[F, E, A]].value) else None
  }

  /**
   * Create an incomplete [[Step]] that will use the given functions to process
   * the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](
    onInput: NonEmptyVector[E] => F[Step[F, E, A]],
    onEnd: F[A]
  )(implicit F: Applicative[F]): Step[F, E, A] = new EffectfulCont[F, E, A] {
    final def feedEl(e: E): F[Step[F, E, A]] = onInput(NonEmptyVector(e, Vector.empty))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onInput(NonEmptyVector(h1, h2 +: t))
    final def run: F[A] = onEnd
  }

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A): Step[F, E, A] = new NoLeftovers(value)

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def doneWithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Vector[E]): Step[F, E, A] =
    remaining match {
      case Vector() => new NoLeftovers(value)
      case Vector(e) => new WithLeftovers(value, Input.el(e))
      case h1 +: h2 +: t => new WithLeftovers(value, Input.chunk(h1, h2, t))
    }

  /**
   * Create a new completed [[Step]] with the given result and leftover [[Input]].
   *
   * @group Constructors
   */
  final def doneWithLeftoverInput[F[_]: Applicative, E, A](value: A, remaining: Input[E]): Step[F, E, A] =
    new WithLeftovers(value, remaining)

  /**
   * Lift an effectful value into a [[Step]].
   *
   * @group Utilities
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): F[Step[F, E, A]] = F.map(fa)(done(_))

  /**
   * Lift an effectful value in a [[cats.Eval]] into a [[Step]].
   *
   * @group Utilities
   */
  final def liftMEval[F[_], E, A](fa: Eval[F[A]])(implicit F: Monad[F]): F[Step[F, E, A]] = F.pure(
    new Cont[F, E, A] {
      final def run: F[A] = fa.value
      final def onEl(e: E): F[Step[F, E, A]] = F.map(fa.value)(a => new WithLeftovers(a, Input.el(e)))
      final def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] =
        F.map(fa.value)(a => new WithLeftovers(a, Input.chunk(h1, h2, t)))
    }
  )

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
  final def fold[F[_], E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Step[F, E, A] =
    new PureCont.WithValue[F, E, A](init) {
      final def onEl(e: E): Step[F, E, A] = self.fold(f(init, e))(f)
      final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, A] =
        self.fold(t.foldLeft(f(f(init, h1), h2))(f))(f)
    }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] =
    new Cont.WithValue[F, E, A](init) {
      final def onEl(e: E): F[Step[F, E, A]] = F.map(f(init, e))(a => foldM(a)(f))
      final def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] =
        F.map(
          t.foldLeft(F.flatMap(f(init, h1))(a => f(a, h2)))((fa, e) => F.flatMap(fa)(a => f(a, e)))
        )(a => foldM(a)(f))
    }

  /**
   * A [[Step]] that collects all the elements in a stream in a vector.
   *
   * @group Collection
   */
  final def consume[F[_]: Applicative, A]: Step[F, A, Vector[A]] = new ConsumeCont(Vector.empty)

  private[this] final class ConsumeCont[F[_], E](acc: Vector[E])(implicit F: Applicative[F])
    extends PureCont.WithValue[F, E, Vector[E]](acc) {
    final def onEl(e: E): Step[F, E, Vector[E]] = new ConsumeCont(acc :+ e)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Vector[E]] = new ConsumeCont(acc ++ (h1 +: h2 +: t))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def consumeIn[F[_], E, C[_]](implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ): Step[F, E, C[E]] = new ConsumeInCont(M.empty)

  private[this] final class ConsumeInCont[F[_], E, C[_]](acc: C[E])(implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ) extends PureCont.WithValue[F, E, C[E]](acc) {
    final def onEl(e: E): Step[F, E, C[E]] = new ConsumeInCont(M.combineK(acc, C.pure(e)))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, C[E]] = new ConsumeInCont(
      t.foldLeft(M.combineK(M.combineK(acc, C.pure(h1)), C.pure(h2)))((a, e) => M.combineK(a, C.pure(e)))
    )
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def onEl(e: E): Step[F, E, Option[E]] = done(Some(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.fromPair(h2, t))
    final def run: F[Option[E]] = F.pure(None)
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def onEl(e: E): Step[F, E, Option[E]] = new WithLeftovers(Some(e), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Option[E]] =
      new WithLeftovers(Some(h1), Input.chunk(h1, h2, t))
    final def run: F[Option[E]] = F.pure(None)
  }

  /**
   * A [[Step]] that counts the number of values in a stream.
   *
   * @group Collection
   */
  final def length[F[_]: Applicative, A]: Step[F, A, Long] = new LengthCont(0L)

  private[this] final class LengthCont[F[_], E](acc: Long)(implicit F: Applicative[F])
    extends PureCont.WithValue[F, E, Long](acc) {
    final def onEl(e: E): Step[F, E, Long] = new LengthCont(acc + 1L)
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Long] = new LengthCont(acc + t.size.toLong + 2L)
  }

  /**
   * A [[Step]] that sums of values in a stream.
   *
   * @group Collection
   */
  final def sum[F[_], E](implicit F: Applicative[F], M: Monoid[E]): Step[F, E, E] = new SumCont(M.empty)

  private[this] final class SumCont[F[_], E](acc: E)(implicit F: Applicative[F], M: Semigroup[E])
    extends PureCont.WithValue[F, E, E](acc) {
    final def onEl(e: E): Step[F, E, E] = new SumCont(M.combine(acc, e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, E] =
      new SumCont(
        M.combine(
          acc,
          M.combine(
            h1,
            M.combineAllOption(t) match {
              case Some(tc) => M.combine(h2, tc)
              case None => h2
            }
          )
        )
      )
  }

  /**
   * A [[Step]] that transforms and sums values in a stream.
   *
   * @group Collection
   */
  final def foldMap[F[_], E, A](f: E => A)(implicit F: Applicative[F], M: Monoid[A]): Step[F, E, A] =
    new FoldMapCont(f, M.empty)

  private[this] final class FoldMapCont[F[_], E, A](f: E => A, acc: A)(implicit F: Applicative[F], M: Semigroup[A])
    extends PureCont.WithValue[F, E, A](acc) {
    final def onEl(e: E): Step[F, E, A] = new FoldMapCont(f, M.combine(acc, f(e)))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, A] =
      new FoldMapCont(
        f,
        M.combine(
          acc,
          M.combine(
            f(h1),
            {
              val h2f = f(h2)

              M.combineAllOption(t.map(f)) match {
                case Some(tc) => M.combine(h2f, tc)
                case None => h2f
              }
            }
          )
        )
      )
  }

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, E](n: Int): Step[F, E, Vector[E]] =
    if (n <= 0) done[F, E, Vector[E]](Vector.empty) else new TakeCont(Vector.empty, n)

  private[this] final class TakeCont[F[_], E](acc: Vector[E], n: Int)(implicit F: Applicative[F])
    extends PureCont.WithValue[F, E, Vector[E]](acc) {
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

  private[this] final class TakeWhileCont[F[_], E](acc: Vector[E], p: E => Boolean)(implicit F: Applicative[F])
    extends PureCont.WithValue[F, E, Vector[E]](acc) {
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
  final def drop[F[_], E](n: Int)(implicit F: Applicative[F]): Step[F, E, Unit] =
    if (n <= 0) done(()) else new PureCont[F, E, Unit] {
      final def onEl(e: E): Step[F, E, Unit] = drop(n - 1)
      final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
        val len = t.size + 2

        if (len <= n) drop(n - len) else {
          val dropped = (h1 +: h2 +: t).drop(n)

          new WithLeftovers((), Input.fromVectorUnsafe(dropped))
        }
      }
      final def run: F[Unit] = F.pure(())
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
    extends PureCont[F, E, Unit] {
    final def onEl(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else new WithLeftovers((), Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Unit] = {
      val after = (h1 +: h2 +: t).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else new WithLeftovers((), Input.fromVectorUnsafe(after))
    }
    final def run: F[Unit] = F.pure(())
  }

  final def isEnd[F[_], E](implicit F: Applicative[F]): Step[F, E, Boolean] = new PureCont[F, E, Boolean] {
    final def onEl(e: E): Step[F, E, Boolean] = new WithLeftovers(false, Input.el(e))
    final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, Boolean] =
      new WithLeftovers(false, Input.chunk(h1, h2, t))
    final def run: F[Boolean] = F.pure(true)
  }
}
