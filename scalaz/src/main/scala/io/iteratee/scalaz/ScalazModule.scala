package io.iteratee.scalaz

import cats.MonadError
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.{ EffectCapture, FileModule }
import scalaz.concurrent.Task

trait ScalazModule extends ScalazInstances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
  with FileModule[Task] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] = scalazTaskMonadError

  final protected val effectCapture: EffectCapture[Task] = new EffectCapture[Task] {
    def apply[A](a: => A): Task[A] = Task.delay(a)
  }
}
