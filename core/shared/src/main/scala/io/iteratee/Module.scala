package io.iteratee

import cats.Monad

trait Module[F[_]] extends EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] {
  final object syntax {
    final implicit class EffectfulValueOps[A](fa: F[A]) {
      final def intoEnumerator(implicit F: Monad[F]): Enumerator[F, A] = liftToEnumerator(fa)
      final def intoIteratee[E](implicit F: Monad[F]): Iteratee[F, E, A] = liftToIteratee(fa)
    }
  }
}
