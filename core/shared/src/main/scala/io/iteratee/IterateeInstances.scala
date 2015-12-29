package io.iteratee

import cats.{ Monad, MonadError }
import cats.functor.Contravariant

private[iteratee] trait IterateeInstances0 {
  implicit final def iterateeMonad[F[_], E](implicit F: Monad[F]): Monad[
    ({ type L[x] = Iteratee[F, E, x] })#L
  ] = new IterateeMonad[F, E](F)
}

trait IterateeInstances extends IterateeInstances0 {
  implicit final def iterateeContravariant[F[_]: Monad, A]: Contravariant[
    ({ type L[x] = Iteratee[F, x, A] })#L
  ] = new Contravariant[({ type L[x] = Iteratee[F, x, A] })#L] {
    def contramap[E, E2](r: Iteratee[F, E, A])(f: E2 => E) = r.contramap(f)
  }

  implicit final def iterateeMonadError[F[_], T, E](implicit F: MonadError[F, T]): MonadError[
    ({ type L[x] = Iteratee[F, E, x] })#L,
    T
  ] = new IterateeMonadError[F, T, E](F)
}

private[iteratee] class IterateeMonad[F[_], E](F: Monad[F])
  extends Monad[({ type L[x] = Iteratee[F, E, x] })#L] {
  final def pure[A](a: A): Iteratee[F, E, A] = Step.done(a, Input.empty).pointI(F)
  override final def map[A, B](fa: Iteratee[F, E, A])(f: A => B): Iteratee[F, E, B] =
    fa.map(f)(F)
  final def flatMap[A, B](fa: Iteratee[F, E, A])(f: A => Iteratee[F, E, B]): Iteratee[F, E, B] =
    fa.flatMap(f)(F)
}

private[iteratee] class IterateeMonadError[F[_], T, E](F: MonadError[F, T])
  extends IterateeMonad[F, E](F) with MonadError[({ type L[x] = Iteratee[F, E, x] })#L, T] {
  final def raiseError[A](e: T): Iteratee[F, E, A] = Iteratee.fail(e)(F)
  final def handleErrorWith[A](fa: Iteratee[F, E, A])(
    f: T => Iteratee[F, E, A]
  ): Iteratee[F, E, A] = fa.handleErrorWith(f)(F)
}
