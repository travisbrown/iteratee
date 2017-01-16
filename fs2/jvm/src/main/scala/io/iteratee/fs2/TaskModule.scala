package io.iteratee.fs2

import cats.MonadError
import fs2.Task
import fs2.interop.cats.Instances
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.SuspendableFileModule

trait TaskModule extends Instances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with SuspendableFileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] = effectToMonadError

  final protected def captureEffect[A](a: => A): Task[A] = Task.delay(a)
}
