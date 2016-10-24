package io.iteratee.scalaz

import cats.MonadError
import scalaz.\/
import scalaz.concurrent.Task

/**
 * @groupname Task Task operations and instances
 * @groupprio Task -1
 */
trait ScalazInstances {
  /**
   * @group Task
   */
  implicit final val scalazTaskMonadError: MonadError[Task, Throwable] = new MonadError[Task, Throwable] {
    final def pure[A](x: A): Task[A] = Task.taskInstance.point(x)
    final def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
    override final def map[A, B](fa: Task[A])(f: A => B): Task[B] = fa.map(f)
    final def raiseError[A](e: Throwable): Task[A] = Task.taskInstance.raiseError(e)
    final def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] = Task.taskInstance.handleError(fa)(f)
    final def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
     Task.taskInstance.tailrecM[A, B](a => f(a).map(\/.fromEither))(a)
  }
}
