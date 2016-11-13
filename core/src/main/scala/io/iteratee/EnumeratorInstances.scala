package io.iteratee

import cats.{ Functor, Monad, Monoid }

private[iteratee] trait EnumeratorInstances {
  implicit final def enumeratorMonoid[F[_]: Monad, E]: Monoid[Enumerator[F, E]] =
    new Monoid[Enumerator[F, E]] {
      def combine(e1: Enumerator[F, E], e2: Enumerator[F, E]): Enumerator[F, E] = e1.append(e2)
      def empty: Enumerator[F, E] = Enumerator.empty
    }

  implicit final def enumeratorMonad[F[_]](implicit M0: Monad[F]): Monad[Enumerator[F, ?]] = new EnumeratorMonad[F] {
    implicit val M: Monad[F] = M0
  }
}

private trait EnumeratorFunctor[F[_]] extends Functor[Enumerator[F, ?]] {
  implicit def M: Monad[F]
  abstract override def map[A, B](fa: Enumerator[F, A])(f: A => B): Enumerator[F, B] = fa.map(f)
}

private trait EnumeratorMonad[F[_]] extends Monad[Enumerator[F, ?]] with EnumeratorFunctor[F] {
  final def flatMap[A, B](fa: Enumerator[F, A])(f: A => Enumerator[F, B]): Enumerator[F, B] = fa.flatMap(f)
  final def pure[E](e: E): Enumerator[F, E] = Enumerator.enumOne[F, E](e)

  /**
   * Note that recursive monadic binding is not stack safe for enumerators.
   */
  final def tailRecM[A, B](a: A)(f: A => Enumerator[F, Either[A, B]]): Enumerator[F, B] = f(a).flatMap {
    case Right(b) => pure(b)
    case Left(nextA) => tailRecM(nextA)(f)
  }
}
