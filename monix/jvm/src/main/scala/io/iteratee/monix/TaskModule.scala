package io.iteratee.monix

import cats.MonadError
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.SuspendableFileModule
import monix.eval.Task

trait TaskModule extends Module[Task]
    with EnumerateeModule[Task]
    with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
    with SuspendableFileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected def captureEffect[A](a: => A): Task[A] = Task.delay(a)
}

final object TaskModule {
  def instance(implicit taskMonadError: MonadError[Task, Throwable]): TaskModule = new TaskModule {
    final protected val F: MonadError[Task, Throwable] = taskMonadError
  }
}
