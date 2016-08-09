package io.iteratee.scalaz

import cats.{Eval, MonadError}
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.FileModule
import scalaz.concurrent.Task

trait ScalazModule extends ScalazInstances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with FileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] = scalazTaskMonadError

  /**
   * Task is already lazy, so we don't need Eval to be lazy here
   */
  final override protected def captureEffect[A](a: => A): Eval[Task[A]] =
    Eval.now(Task(a))
}
