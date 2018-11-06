package io.iteratee.scalaz

import cats.effect.Sync
import io.iteratee.modules.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.modules.FileModule
import scalaz.concurrent.Task

trait TaskModule
    extends ScalazInstances
    with Module[Task]
    with EnumerateeModule[Task]
    with EnumeratorErrorModule[Task, Throwable]
    with IterateeErrorModule[Task, Throwable]
    with FileModule[Task] {
  final type M[f[_]] = Sync[f]

  final protected val F: Sync[Task] = scalazTaskSync
}
