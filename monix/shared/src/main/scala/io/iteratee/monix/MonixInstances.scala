package io.iteratee.monix

import cats.effect.{ ExitCase, Sync }
import monix.eval.Task

/**
 * Type class instances for Monix types.
 *
 * Note that in most cases you should use the instances provided by monix-cats.
 * This trait is provided for convenience when monix-cats is not available.
 */
trait MonixInstances {
  implicit final val monixTaskSync: Sync[Task] = new Sync[Task] {
    final def pure[A](x: A): Task[A] = Task.now(x)
    final def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
    override final def map[A, B](fa: Task[A])(f: A => B): Task[B] = fa.map(f)
    final def raiseError[A](e: Throwable): Task[A] = Task.raiseError(e)
    final def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] = fa.onErrorHandleWith(f)
    final def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] = f(a).flatMap {
      case Right(b)    => pure(b)
      case Left(nextA) => tailRecM(nextA)(f)
    }

    final def bracketCase[A, B](acquire: Task[A])(use: A => Task[B])(
      release: (A, ExitCase[Throwable]) => Task[Unit]
    ): Task[B] = acquire.bracketE(use) {
      case (a, Right(_))      => release(a, ExitCase.complete)
      case (a, Left(None))    => release(a, ExitCase.canceled)
      case (a, Left(Some(e))) => release(a, ExitCase.error(e))
    }

    final def suspend[A](thunk: => Task[A]): Task[A] = Task.defer(thunk)
  }
}
