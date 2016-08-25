package io.iteratee.scalaz

import cats.MonadError
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.SuspendableFileModule
import scalaz.concurrent.Task

trait TaskModule extends ScalazInstances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with SuspendableFileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] = scalazTaskMonadError

  final protected def captureEffect[A](a: => A): Task[A] = Task.delay(a)
}
