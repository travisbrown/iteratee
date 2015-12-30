package io.iteratee

import cats.Monad

/**
 * @groupname Syntax Extension methods
 * @groupprio Syntax 3
 */
trait Module[F[_]] extends EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] {
  /**
   * @group Syntax
   */
  final object syntax {
    final implicit class EffectfulValueOps[A](fa: F[A]) {
      final def intoEnumerator(implicit F: Monad[F]): Enumerator[F, A] = Enumerator.liftM(fa)
      final def intoIteratee[E](implicit F: Monad[F]): Iteratee[F, E, A] = Iteratee.liftM(fa)
    }
  }
}
