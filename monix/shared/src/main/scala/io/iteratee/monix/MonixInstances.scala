package io.iteratee.monix

import cats.MonadError
import monix.eval.Task

/**
 * Type class instances for Monix types.
 *
 * Note that in most cases you should use the instances provided by monix-cats.
 * This trait is provided for convenience when monix-cats is not available.
 */
trait MonixInstances {
  implicit final val monixTaskMonadError: MonadError[Task, Throwable] = new MonadError[Task, Throwable] {
    final def pure[A](x: A): Task[A] = Task.now(x)
    final def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
    override final def map[A, B](fa: Task[A])(f: A => B): Task[B] = fa.map(f)
    final def raiseError[A](e: Throwable): Task[A] = Task.raiseError(e)
    final def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] = fa.onErrorHandleWith(f)
    final def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] = f(a).flatMap {
      case Right(b) => pure(b)
      case Left(nextA) => tailRecM(nextA)(f)
    }
  }
}
