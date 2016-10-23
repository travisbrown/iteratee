package io.iteratee.monix

import cats.MonadError
import monix.eval.Task

trait DefaultTaskModule extends TaskModule with MonixInstances {
  final protected val F: MonadError[Task, Throwable] = monixTaskMonadError
}
