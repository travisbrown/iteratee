package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad, MonadError, MonoidK }
import cats.data.NonEmptyVector
import io.iteratee.internal.Step
import scala.collection.generic.CanBuildFrom

/**
 * @groupname Iteratees Iteratees
 * @groupprio Iteratees 1
 *
 * @groupname Helpers Helper classes
 * @groupprio Helpers 4
 */
trait IterateeModule[F[_]] {
  /**
   * Create an incomplete [[Iteratee]] that will use the given function to
   * process the next input.
   *
   * @group Constructors
   */
  final def cont[E, A](
    ifInput: NonEmptyVector[E] => Iteratee[F, E, A],
    ifEnd: F[A]
  )(implicit F: Applicative[F]): Iteratee[F, E, A] = Iteratee.cont(ifInput, ifEnd)

  /**
   * Create a new completed [[Iteratee]] with the given result and leftover
   * input.
   *
   * @group Constructors
   */
  final def done[E, A](value: A, remaining: Vector[E] = Vector.empty)
    (implicit F: Applicative[F]): Iteratee[F, E, A] =
      Iteratee.done(value, remaining)

  /**
   * Create a new completed [[Iteratee]] with the given result and leftover
   * input.
   *
   * @group Constructors
   */
  final def ended[F[_], E, A](value: A)(implicit F: Applicative[F]): Iteratee[F, E, A] = Iteratee.ended(value)

  /**
   * @group Helpers
   */
  sealed class LiftToIterateePartiallyApplied[E] {
    final def apply[A](fa: F[A])(implicit F: Monad[F]): Iteratee[F, E, A] = Iteratee.liftM(fa)
  }

  /**
   * Lift an effectful value into an iteratee.
   *
   * @group Iteratees
   */
  final def liftToIteratee[E]: LiftToIterateePartiallyApplied[E] = new LiftToIterateePartiallyApplied[E]

  /**
   * @group Helpers
   */
  sealed class FailIterateePartiallyApplied[E, A] {
    final def apply[T](e: T)(implicit F: MonadError[F, T]): Iteratee[F, E, A] = Iteratee.fail(e)
  }

  /**
   * Create a failed iteratee with the given error.
   *
   * @group Iteratees
   */
  final def failIteratee[E, A]: FailIterateePartiallyApplied[E, A] = new FailIterateePartiallyApplied[E, A]

  /**
   * An iteratee that reads nothing from a stream.
   *
   * @group Iteratees
   */
  final def identity[E](implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.identity.up[F]

  /**
   * An [[Iteratee]] that folds a stream using an initial value and an
   * accumulation function.
   *
   * @group Iteratees
   */
  final def fold[E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Iteratee[F, E, A] =
    Iteratee.fold(init)(f).up[F]

  /**
   * An [[Iteratee]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Iteratees
   */
  final def foldM[E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.foldM(init)(f)

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a vector.
   *
   * @group Iteratees
   */
  final def drain[E](implicit F: Monad[F]): Iteratee[F, E, Vector[E]] = Iteratee.drain.up[F]

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a given
   * collection type.
   *
   * @group Iteratees
   */
  final def drainTo[E, C[_]: Applicative: MonoidK](implicit F: Applicative[F]): Iteratee[F, E, C[E]] =
    Iteratee.drainTo[E, C].up[F]

  /**
   * An [[Iteratee]] that returns the first value in a stream.
   *
   * @group Iteratees
   */
  final def head[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.head.up[F]

  /**
   * An [[Iteratee]] that returns the first value in a stream without consuming
   * it.
   *
   * @group Iteratees
   */
  final def peek[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.peek.up[F]

  /**
   * An [[Iteratee]] that returns a given number of the first values in a
   * stream.
   *
   * @group Iteratees
   */
  final def take[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] = Iteratee.take(n).up[F]

  /**
   * An [[Iteratee]] that returns values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Iteratees
   */
  final def takeWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] =
    Iteratee.takeWhile(p).up[F]

  /**
   * An [[Iteratee]] that drops a given number of the values from a stream.
   *
   * @group Iteratees
   */
  final def drop[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.drop(n).up[F]

  /**
   * An [[Iteratee]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Iteratees
   */
  final def dropWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Unit] =
    Iteratee.dropWhile(p).up[F]

  /**
   * An [[Iteratee]] that collects all inputs in reverse order.
   *
   * @group Iteratees
   */
  final def reversed[E](implicit F: Applicative[F]): Iteratee[F, E, List[E]] = Iteratee.reversed.up[F]

  /**
   * An [[Iteratee]] that counts the number of values in a stream.
   *
   * @group Iteratees
   */
  final def length[E](implicit F: Applicative[F]): Iteratee[F, E, Int] = Iteratee.length.up[F]

  /**
   * An [[Iteratee]] that combines values using an [[algebra.Monoid]] instance.
   *
   * @group Iteratees
   */
  final def sum[E: Monoid](implicit F: Monad[F]): Iteratee[F, E, E] = Iteratee.sum[E].up[F]

  /**
   * An [[Iteratee]] that combines values using a function to a type with an
   * [[algebra.Monoid]] instance.
   *
   * @group Iteratees
   */
  final def foldMap[E, A](f: E => A)(implicit F: Monad[F], A: Monoid[A]): Iteratee[F, E, A] =
    Iteratee.foldMap(f).up[F]

  /**
   * An [[Iteratee]] that checks if the stream is at its end.
   *
   * @group Iteratees
   */
  final def isEnd[E](implicit F: Applicative[F]): Iteratee[F, E, Boolean] = Iteratee.isEnd.up[F]
}
