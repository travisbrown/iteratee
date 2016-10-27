package io.iteratee

import cats.{ Monad, MonadError }
import cats.functor.Contravariant
import io.iteratee.internal.{ NoLeftovers, Step, WithLeftovers }

private[iteratee] trait IterateeInstances extends IterateeInstances0 {
  implicit final def iterateeContravariant[F[_]: Monad, A]: Contravariant[({ type L[x] = Iteratee[F, x, A] })#L] =
    new Contravariant[({ type L[x] = Iteratee[F, x, A] })#L] {
      def contramap[E, E2](r: Iteratee[F, E, A])(f: E2 => E) = r.contramap(f)
    }

  implicit final def iterateeMonadError[F[_], T, E](implicit
    F: MonadError[F, T]
  ): MonadError[({ type L[x] = Iteratee[F, E, x] })#L, T] =
    new IterateeMonadError[F, T, E]
}

private[iteratee] trait IterateeInstances0 {
  implicit final def iterateeMonad[F[_], E](implicit F: Monad[F]): Monad[({ type L[x] = Iteratee[F, E, x] })#L] =
    new IterateeMonad[F, E]
}

private class IterateeMonad[F[_], E](implicit F: Monad[F]) extends Monad[({ type L[x] = Iteratee[F, E, x] })#L] {
  final def pure[A](a: A): Iteratee[F, E, A] = Iteratee.fromStep(Step.done[F, E, A](a))
  override final def map[A, B](fa: Iteratee[F, E, A])(f: A => B): Iteratee[F, E, B] = fa.map(f)
  final def flatMap[A, B](fa: Iteratee[F, E, A])(f: A => Iteratee[F, E, B]): Iteratee[F, E, B] = fa.flatMap(f)

  private[this] class TailRecStep[A, B](f: A => F[Step[F, E, Either[A, B]]])(
    s: Step[F, E, Either[A, B]]
  ) extends Step.Cont[F, E, B] {
    final def run: F[B] = F.tailRecM(s)(s =>
      F.flatMap(s.run) {
        case Right(b) => F.pure(Right(b))
        case Left(a) => F.map(f(a))(Left(_))
      }
    )

    private[this] def loop(s: Step[F, E, Either[A, B]]): F[Either[Step[F, E, Either[A, B]], Step[F, E, B]]] =
      s match {
        case NoLeftovers(Right(b)) => F.pure(Right(Step.done(b)))
        case WithLeftovers(Right(b), remaining) => F.pure(Right(Step.doneWithLeftovers(b, remaining.toVector)))
        case NoLeftovers(Left(a)) => F.map(f(a))(s => Right(new TailRecStep(f)(s)))
        case WithLeftovers(Left(a), remaining) if remaining.size == 1 =>
          F.flatMap(f(a))(s => F.map(s.feedEl(remaining.toVector.head))(Left(_)))
        case WithLeftovers(Left(a), remaining) =>
          val v = remaining.toVector

          F.flatMap(f(a))(s => F.map(s.feedChunk(v(0), v(1), v.drop(2)))(Left(_)))
        case cont => F.pure(Right(new TailRecStep(f)(cont)))
      }

    def onEl(e: E): F[Step[F, E, B]] = F.flatMap(s.feedEl(e))(F.tailRecM(_)(loop))
    def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = F.flatMap(s.feedChunk(h1, h2, t))(F.tailRecM(_)(loop))
  }

  final def tailRecM[A, B](a: A)(f: A => Iteratee[F, E, Either[A, B]]): Iteratee[F, E, B] = Iteratee.iteratee(
    F.map(f(a).state)(new TailRecStep[A, B](a => f(a).state)(_))
  )
}

private class IterateeMonadError[F[_], T, E](implicit F: MonadError[F, T])
    extends IterateeMonad[F, E] with MonadError[({ type L[x] = Iteratee[F, E, x] })#L, T] {
  final def raiseError[A](e: T): Iteratee[F, E, A] = Iteratee.fail(e)(F)
  final def handleErrorWith[A](fa: Iteratee[F, E, A])(f: T => Iteratee[F, E, A]): Iteratee[F, E, A] =
    fa.handleErrorWith(f)(F)
}
