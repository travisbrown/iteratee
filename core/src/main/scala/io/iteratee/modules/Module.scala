package io.iteratee.modules

import cats.Monad
import io.iteratee.{Enumerator, Iteratee}

/**
 * @groupname Syntax
 *   Extension methods
 * @groupprio Syntax
 *   3
 */
trait Module[F[_]] {
  type M[f[_]] <: Monad[f]

  protected def F: M[F]

  /**
   * @group Syntax
   */
  final object syntax {
    final implicit class EffectfulValueOps[A](fa: F[A]) {
      final def intoEnumerator: Enumerator[F, A] = Enumerator.liftM(fa)(F)
      final def intoIteratee[E]: Iteratee[F, E, A] = Iteratee.liftM(fa)(F)
    }
  }
}

object Module {
  private[this] class FromMonad[F[_]](monad: Monad[F]) extends Module[F] {
    type M[F[T]] = Monad[F]
    def F: Monad[F] = monad
  }

  def apply[F[_]](implicit monad: Monad[F]): Module[F] =
    new FromMonad(monad)
}
