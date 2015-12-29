package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad }
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

private[iteratee] trait CollectionIteratees {
  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Iteratee[F, E, A] =
    Step.fold[F, E, A](init)(f).pointI

  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Step.foldM[F, E, A](init)(f).pointI

  final def consumeIn[F[_], A, C[_]](implicit
    F: Monad[F],
    cbf: CanBuildFrom[Nothing, A, C[A]]
  ): Iteratee[F, A, C[A]] = Step.consumeIn[F, A, C].pointI

  final def consume[F[_], A](implicit F: Applicative[F]): Iteratee[F, A, Vector[A]] =
    Step.consume[F, A].pointI

  /**
   * An iteratee that consumes the head of the input.
   */
  final def head[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = Step.head[F, E].pointI

  /**
   * An iteratee that returns the first element of the input.
   */
  final def peek[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = Step.peek[F, E].pointI

  /**
   * Iteratee that collects the first `n` inputs.
   */
  final def take[F[_]: Applicative, A](n: Int): Iteratee[F, A, Vector[A]] =
    Step.take[F, A](n).pointI

  /**
   * Iteratee that collects inputs until the input element fails a test.
   */
  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Iteratee[F, A, Vector[A]] =
    Step.takeWhile[F, A](p).pointI

  /**
   * An iteratee that skips the first `n` elements of the input.
   */
  final def drop[F[_]: Applicative, E](n: Int): Iteratee[F, E, Unit] = Step.drop[F, E](n).pointI

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Iteratee[F, E, Unit] =
    Step.dropWhile[F, E](p).pointI

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for `F[_]` with efficient cons, e.g. `List`.
   */
  final def reversed[F[_]: Applicative, A]: Iteratee[F, A, List[A]] =
    Iteratee.fold[F, A, List[A]](Nil)((acc, e) => e :: acc)

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  final def length[F[_]: Applicative, E]: Iteratee[F, E, Int] = fold(0)((a, _) => a + 1)

  /**
   * An iteratee that checks if the input is EOF.
   */
  final def isEnd[F[_]: Applicative, E]: Iteratee[F, E, Boolean] =
    Iteratee.cont(in => Iteratee.done(in.isEnd, in))

  final def sum[F[_], E](implicit F: Monad[F], E: Monoid[E]): Iteratee[F, E, E] =
    fold(E.empty)((a, e) => E.combine(a, e))

  final def foldMap[F[_], E, A](f: E => A)(implicit F: Monad[F], A: Monoid[A]): Iteratee[F, E, A] =
    fold(A.empty)((a, e) => A.combine(a, f(e)))
}
