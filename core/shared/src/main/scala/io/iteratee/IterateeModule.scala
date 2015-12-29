package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad, MonadError, MonoidK }
import scala.collection.generic.CanBuildFrom

trait IterateeModule[F[_]] {
  class LiftToIterateePartiallyApplied[E] {
    def apply[A](fa: F[A])(implicit F: Monad[F]): Iteratee[F, E, A] = Iteratee.liftM(fa)
  }

  /**
   * Lift an effectful value into an iteratee.
   */
  final def liftToIteratee[E]: LiftToIterateePartiallyApplied[E] =
    new LiftToIterateePartiallyApplied[E]

  class FailIterateePartiallyApplied[E, A] {
    def apply[T](e: T)(implicit F: MonadError[F, T]): Iteratee[F, E, A] = Iteratee.fail(e)
  }

  final def failIteratee[E, A]: FailIterateePartiallyApplied[E, A] =
    new FailIterateePartiallyApplied[E, A]

  final def identity[E](implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.identity

  final def consume[E](implicit F: Monad[F]): Iteratee[F, E, Vector[E]] = Iteratee.consume

  /**
   * An iteratee that consumes all of the input into something that is MonoidK and Applicative.
   */
  final def consumeIn[E, C[_]](implicit
    F: Monad[F],
    cbf: CanBuildFrom[Nothing, E, C[E]]
  ): Iteratee[F, E, C[E]] = Iteratee.consumeIn

  /**
   * An iteratee that consumes the head of the input.
   */
  final def head[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.head

  /**
   * An iteratee that returns the first element of the input.
   */
  final def peek[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.peek

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for `F[_]` with efficient cons, e.g. `List`.
   */
  final def reversed[E](implicit F: Applicative[F]): Iteratee[F, E, List[E]] = Iteratee.reversed

  /**
   * Iteratee that collects the first `n` inputs.
   */
  final def take[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] =
    Iteratee.take(n)

  /**
   * Iteratee that collects inputs until the input element fails a test.
   */
  final def takeWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] =
    Iteratee.takeWhile(p)

  /**
   * An iteratee that skips the first `n` elements of the input.
   */
  final def drop[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.drop(n)

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  final def dropWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Unit] =
    Iteratee.dropWhile(p)

  final def fold[E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Iteratee[F, E, A] =
    Iteratee.fold(init)(f)

  final def foldM[E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.foldM(init)(f)

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  final def length[E](implicit F: Applicative[F]): Iteratee[F, E, Int] = Iteratee.length

  final def sum[E: Monoid](implicit F: Monad[F]): Iteratee[F, E, E] = Iteratee.sum

  final def foldMap[E, A](f: E => A)(implicit F: Monad[F], A: Monoid[A]): Iteratee[F, E, A] =
    Iteratee.foldMap(f)

  /**
   * An iteratee that checks if the input is EOF.
   */
  final def isEnd[E](implicit F: Applicative[F]): Iteratee[F, E, Boolean] = Iteratee.isEnd
}
