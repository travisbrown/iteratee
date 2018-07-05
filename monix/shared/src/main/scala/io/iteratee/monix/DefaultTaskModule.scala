package io.iteratee.monix

import cats.effect.Sync
import monix.eval.Task

trait DefaultTaskModule extends TaskModule with MonixInstances {
  final protected val F: Sync[Task] = monixTaskSync
}
