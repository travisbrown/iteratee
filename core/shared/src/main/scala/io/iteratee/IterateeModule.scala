package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad, MonoidK }

trait IterateeModule[F[_]] {
  /**
   * Lift an effectful value into an iteratee.
   */
  final def liftToIteratee[E, A](fa: F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.liftM[F, E, A](fa)

  final def identity[E](implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.identity[F, E]

  /**
   * An iteratee that consumes all of the input into something that is MonoidK and Applicative.
   */
  final def consumeIn[E, A[_]: MonoidK: Applicative](implicit F: Monad[F]): Iteratee[F, E, A[E]] =
    Iteratee.consumeIn[F, E, A]

  final def consume[E](implicit F: Monad[F]): Iteratee[F, E, Vector[E]] = Iteratee.consume[F, E]

  final def collectT[E, A[_]](implicit
    M: Monad[F],
    mae: Monoid[A[E]],
    pointed: Applicative[A]
  ): Iteratee[F, E, A[E]] = Iteratee.collectT[F, E, A]

  /**
   * An iteratee that consumes the head of the input.
   */
  final def head[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.head[F, E]

  /**
   * An iteratee that returns the first element of the input.
   */
  final def peek[E](implicit F: Applicative[F]): Iteratee[F, E, Option[E]] = Iteratee.peek[F, E]

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for `F[_]` with efficient cons, e.g. `List`.
   */
  final def reversed[E](implicit F: Applicative[F]): Iteratee[F, E, List[E]] = Iteratee.reversed[F, E]

  /**
   * Iteratee that collects the first `n` inputs.
   */
  final def take[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] =
    Iteratee.take[F, E](n)

  /**
   * Iteratee that collects inputs until the input element fails a test.
   */
  final def takeWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Vector[E]] =
    Iteratee.takeWhile[F, E](p)

  /**
   * An iteratee that skips the first `n` elements of the input.
   */
  final def drop[E](n: Int)(implicit F: Applicative[F]): Iteratee[F, E, Unit] = Iteratee.drop[F, E](n)

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  final def dropWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[F, E, Unit] =
    Iteratee.dropWhile[F, E](p)

  final def fold[E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Iteratee[F, E, A] =
    Iteratee.fold[F, E, A](init)(f)

  final def foldM[E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.foldM[F, E, A](init)(f)

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  final def length[E](implicit F: Applicative[F]): Iteratee[F, E, Int] = Iteratee.length[F, E]

  /**
   * An iteratee that checks if the input is EOF.
   */
  final def isEnd[E](implicit F: Applicative[F]): Iteratee[F, E, Boolean] = Iteratee.isEnd[F, E]

  final def sum[E: Monoid](implicit F: Monad[F]): Iteratee[F, E, E] = Iteratee.sum[F, E]
}
