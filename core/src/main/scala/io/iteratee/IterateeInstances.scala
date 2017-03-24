package io.iteratee

import cats.{ Monad, MonadError }
import cats.functor.Contravariant
import io.iteratee.internal.Step

private[iteratee] trait IterateeInstances extends IterateeInstances0 {
  implicit final def iterateeContravariant[F[_]: Monad, A]: Contravariant[Iteratee[F, ?, A]] =
    new Contravariant[Iteratee[F, ?, A]] {
      def contramap[E, E2](r: Iteratee[F, E, A])(f: E2 => E) = r.contramap(f)
    }

  implicit final def iterateeMonadError[F[_], T, E](implicit F: MonadError[F, T]): MonadError[Iteratee[F, E, ?], T] =
    new IterateeMonadError[F, T, E]
}

private[iteratee] trait IterateeInstances0 {
  implicit final def iterateeMonad[F[_], E](implicit F: Monad[F]): Monad[Iteratee[F, E, ?]] = new IterateeMonad[F, E]
}

private class IterateeMonad[F[_], E](implicit F: Monad[F]) extends Monad[Iteratee[F, E, ?]] {
  final def pure[A](a: A): Iteratee[F, E, A] = Iteratee.fromStep(Step.done[F, E, A](a))
  override final def map[A, B](fa: Iteratee[F, E, A])(f: A => B): Iteratee[F, E, B] = fa.map(f)
  final def flatMap[A, B](fa: Iteratee[F, E, A])(f: A => Iteratee[F, E, B]): Iteratee[F, E, B] = fa.flatMap(f)

  final def tailRecM[A, B](a: A)(f: A => Iteratee[F, E, Either[A, B]]): Iteratee[F, E, B] = Iteratee.fromStep(
    Step.tailRecM[F, E, A, B](a)(f(_).state)
  )
}

private class IterateeMonadError[F[_], T, E](implicit F: MonadError[F, T])
    extends IterateeMonad[F, E] with MonadError[Iteratee[F, E, ?], T] {
  final def raiseError[A](e: T): Iteratee[F, E, A] = Iteratee.fail(e)(F)
  final def handleErrorWith[A](fa: Iteratee[F, E, A])(f: T => Iteratee[F, E, A]): Iteratee[F, E, A] =
    fa.handleErrorWith(f)(F)
}
