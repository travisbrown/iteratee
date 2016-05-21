package io.iteratee.monix

import cats.{ Eval, MonadError }
import _root_.monix.eval.Task

/**
 * @groupname Task Task operations and instances
 * @groupprio Task -1
 */
trait MonixInstances {
  /**
   * @group Task
   */
  implicit final val monixTaskMonadError: MonadError[Task, Throwable] = new MonadError[Task, Throwable] {
    final def pure[A](x: A): Task[A] = Task.now(x)
    final override def pureEval[A](x: Eval[A]): Task[A] = Task(x.value)
    final def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
    final override def map[A, B](fa: Task[A])(f: A => B): Task[B] = fa.map(f)
    final def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] = fa.onErrorHandleWith(f)
    final def raiseError[A](e: Throwable): Task[A] = Task.raiseError(e)
  }
}
