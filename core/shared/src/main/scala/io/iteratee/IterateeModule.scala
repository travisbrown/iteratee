package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad, MonoidK }

trait IterateeModule[F[_]] {
  /**
   * Lift an effectful value into an iteratee.
   */
  final def liftToIteratee[E, A](fa: F[A])(implicit F: Monad[F]): Iteratee[E, F, A] =
    Iteratee.liftM[E, F, A](fa)

  final def identity[E](implicit F: Applicative[F]): Iteratee[E, F, Unit] = Iteratee.identity[E, F]

  /**
   * An iteratee that consumes all of the input into something that is MonoidK and Applicative.
   */
  final def consumeIn[E, A[_]: MonoidK: Applicative](implicit F: Monad[F]): Iteratee[E, F, A[E]] =
    Iteratee.consumeIn[E, F, A]

  final def consume[E](implicit F: Monad[F]): Iteratee[E, F, Vector[E]] = Iteratee.consume[E, F]

  final def collectT[E, A[_]](implicit
    M: Monad[F],
    mae: Monoid[A[E]],
    pointed: Applicative[A]
  ): Iteratee[E, F, A[E]] = Iteratee.collectT[E, F, A]

  /**
   * An iteratee that consumes the head of the input.
   */
  final def head[E](implicit F: Applicative[F]): Iteratee[E, F, Option[E]] = Iteratee.head[E, F]

  /**
   * An iteratee that returns the first element of the input.
   */
  final def peek[E](implicit F: Applicative[F]): Iteratee[E, F, Option[E]] = Iteratee.peek[E, F]

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for `F[_]` with efficient cons, e.g. `List`.
   */
  final def reversed[E](implicit F: Applicative[F]): Iteratee[E, F, List[E]] = Iteratee.reversed[E, F]

  /**
   * Iteratee that collects the first `n` inputs.
   */
  final def take[E](n: Int)(implicit F: Applicative[F]): Iteratee[E, F, Vector[E]] =
    Iteratee.take[E, F](n)

  /**
   * Iteratee that collects inputs until the input element fails a test.
   */
  final def takeWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[E, F, Vector[E]] =
    Iteratee.takeWhile[E, F](p)

  /**
   * An iteratee that skips the first `n` elements of the input.
   */
  final def drop[E](n: Int)(implicit F: Applicative[F]): Iteratee[E, F, Unit] = Iteratee.drop[E, F](n)

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  final def dropWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Iteratee[E, F, Unit] =
    Iteratee.dropWhile[E, F](p)

  final def fold[E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Iteratee[E, F, A] =
    Iteratee.fold[E, F, A](init)(f)

  final def foldM[E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[E, F, A] =
    Iteratee.foldM[E, F, A](init)(f)

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  final def length[E](implicit F: Applicative[F]): Iteratee[E, F, Int] = Iteratee.length[E, F]

  /**
   * An iteratee that checks if the input is EOF.
   */
  final def isEnd[E](implicit F: Applicative[F]): Iteratee[E, F, Boolean] = Iteratee.isEnd[E, F]

  final def sum[E: Monoid](implicit F: Monad[F]): Iteratee[E, F, E] = Iteratee.sum[E, F]
}
