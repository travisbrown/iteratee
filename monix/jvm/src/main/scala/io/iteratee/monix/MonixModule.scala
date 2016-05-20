package io.iteratee.monix

import cats.MonadError
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.FileModule
import monix.eval.Task

trait MonixModule extends MonixInstances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with FileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] = monixTaskMonadError

  final protected def captureEffect[A](a: => A): Task[A] = Task(a)
}
