package io.iteratee

import cats.{ Applicative, MonadError, Monoid, MonoidK, Semigroup }
import cats.data.NonEmptyList

/**
 * @groupname Iteratees Iteratees
 * @groupprio Iteratees 1
 *
 * @groupname Helpers Helper classes
 * @groupprio Helpers 4
 */
trait IterateeModule[F[_]] { self: Module[F] =>
  /**
   * Create an incomplete [[Iteratee]] that will use the given function to
   * process the next input.
   *
   * @group Constructors
   */
  final def cont[E, A](ifInput: NonEmptyList[E] => Iteratee[F, E, A], ifEnd: F[A]): Iteratee[F, E, A] =
    Iteratee.cont(ifInput, ifEnd)(F)

  /**
   * Create a new completed [[Iteratee]] with the given result.
   *
   * @group Constructors
   */
  final def done[E, A](value: A): Iteratee[F, E, A] = Iteratee.done(value)(F)

  /**
   * @group Helpers
   */
  sealed class LiftToIterateePartiallyApplied[E] {
    final def apply[A](fa: F[A]): Iteratee[F, E, A] = Iteratee.liftM(fa)(F)
  }

  /**
   * Lift an effectful value into an iteratee.
   *
   * @group Iteratees
   */
  final def liftToIteratee[E]: LiftToIterateePartiallyApplied[E] = new LiftToIterateePartiallyApplied[E]

  /**
   * An iteratee that reads nothing from a stream.
   *
   * @group Iteratees
   */
  final def identityIteratee[E]: Iteratee[F, E, Unit] = Iteratee.identity(F)

  /**
   * An [[Iteratee]] that folds a stream using an initial value and an
   * accumulation function.
   *
   * @group Iteratees
   */
  final def fold[E, A](init: A)(f: (A, E) => A): Iteratee[F, E, A] = Iteratee.fold(init)(f)(F)

  /**
   * An [[Iteratee]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Iteratees
   */
  final def foldM[E, A](init: A)(f: (A, E) => F[A]): Iteratee[F, E, A] = Iteratee.foldM(init)(f)(F)

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a vector.
   *
   * @group Iteratees
   */
  final def consume[E]: Iteratee[F, E, Vector[E]] = Iteratee.consume(F)

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a given
   * collection type.
   *
   * @group Iteratees
   */
  final def consumeIn[E, C[_]](implicit C: Applicative[C], M: MonoidK[C]): Iteratee[F, E, C[E]] =
    Iteratee.consumeIn(F, C, M)

  /**
   * An [[Iteratee]] that returns the first value in a stream.
   *
   * @group Iteratees
   */
  final def head[E]: Iteratee[F, E, Option[E]] = Iteratee.head(F)

  /**
   * An [[Iteratee]] that returns the first value in a stream without consuming
   * it.
   *
   * @group Iteratees
   */
  final def peek[E]: Iteratee[F, E, Option[E]] = Iteratee.peek(F)

  /**
   * An [[Iteratee]] that returns the last value in a stream.
   *
   * @group Iteratees
   */
  final def last[E]: Iteratee[F, E, Option[E]] = Iteratee.last(F)

  /**
   * An [[Iteratee]] that returns a given number of the first values in a
   * stream.
   *
   * @group Iteratees
   */
  final def takeI[E](n: Int): Iteratee[F, E, Vector[E]] = Iteratee.take(n)(F)

  /**
   * An [[Iteratee]] that returns values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Iteratees
   */
  final def takeWhileI[E](p: E => Boolean): Iteratee[F, E, Vector[E]] = Iteratee.takeWhile(p)(F)

  /**
   * An [[Iteratee]] that drops a given number of the values from a stream.
   *
   * @group Iteratees
   */
  final def dropI[E](n: Int): Iteratee[F, E, Unit] = Iteratee.drop(n)(F)

  /**
   * An [[Iteratee]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Iteratees
   */
  final def dropWhileI[E](p: E => Boolean): Iteratee[F, E, Unit] = Iteratee.dropWhile(p)(F)

  /**
   * An [[Iteratee]] that collects all inputs in reverse order.
   *
   * @group Iteratees
   */
  final def reversed[E]: Iteratee[F, E, List[E]] = Iteratee.reversed(F)

  /**
   * An [[Iteratee]] that counts the number of values in a stream.
   *
   * @group Iteratees
   */
  final def length[E]: Iteratee[F, E, Long] = Iteratee.length(F)

  /**
   * An [[Iteratee]] that combines values using an [[cats.Monoid]] instance.
   *
   * @group Iteratees
   */
  final def sum[E](implicit E: Monoid[E]): Iteratee[F, E, E] = Iteratee.sum(F, E)

  /**
   * An [[Iteratee]] that combines values using a function to a type with a
   * [[cats.Monoid]] instance.
   *
   * @group Iteratees
   */
  final def foldMap[E, A](f: E => A)(implicit A: Monoid[A]): Iteratee[F, E, A] = Iteratee.foldMap(f)(F, A)

  /**
   * An [[Iteratee]] that combines values using an effectful function to a type
   * with a [[cats.Monoid]] instance.
   *
   * @group Iteratees
   */
  final def foldMapM[E, A](f: E => F[A])(implicit A: Monoid[A]): Iteratee[F, E, A] = Iteratee.foldMapM(f)(F, A)

  /**
   * An [[Iteratee]] that combines values using a function to a type with a
   * [[cats.Semigroup]] instance.
   *
   * @group Iteratees
   */
  final def foldMapOption[E, A](f: E => A)(implicit A: Semigroup[A]): Iteratee[F, E, Option[A]] =
    Iteratee.foldMapOption(f)(F, A)

  /**
   * An [[Iteratee]] that combines values using an effectful function to a type
   * with a [[cats.Semigroup]] instance.
   *
   * @group Iteratees
   */
  final def foldMapMOption[E, A](f: E => F[A])(implicit A: Semigroup[A]): Iteratee[F, E, Option[A]] =
    Iteratee.foldMapMOption(f)(F, A)

  /**
   * An [[Iteratee]] that checks if the stream is at its end.
   *
   * @group Iteratees
   */
  final def isEnd[E]: Iteratee[F, E, Boolean] = Iteratee.isEnd(F)

  /**
   * An [[Iteratee]] that runs a function for its side effects.
   *
   * @group Iteratees
   */
  final def foreach[E](f: E => Unit): Iteratee[F, E, Unit] = Iteratee.foreach(f)(F)

  /**
   * An [[Iteratee]] that runs an effectful function for its side effects.
   *
   * @group Iteratees
   */
  final def foreachM[A](f: A => F[Unit]): Iteratee[F, A, Unit] = Iteratee.foreachM(f)(F)
}

trait IterateeErrorModule[F[_], T] extends IterateeModule[F] {
  this: Module[F] { type M[f[_]] <: MonadError[f, T] } =>

  /**
   * Create a failed iteratee with the given error.
   *
   * @group Iteratees
   */
  final def failIteratee[E, A](t: T): Iteratee[F, E, A] = Iteratee.fail(t)(F)
}
