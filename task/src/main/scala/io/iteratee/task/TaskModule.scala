package io.iteratee.task

import cats.MonadError
import io.iteratee.Module
import io.iteratee.files.FileModule
import scalaz.concurrent.Task

trait TaskModule extends TaskInstances with Module[Task] with FileModule[Task] {
  final protected val fileModuleF: MonadError[Task, Throwable] = taskMonadError

  final protected def captureEffect[A](a: => A): Task[A] = Task(a)
}
