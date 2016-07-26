package io.iteratee.monix

import cats.MonadError
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.FileModule
import monix.eval.Task
import monix.cats._

trait MonixModule extends Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with FileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] =
    monixMonadErrorInstancesToCats[Task, Throwable]

  final protected def captureEffect[A](a: => A): Task[A] = Task(a)
}
