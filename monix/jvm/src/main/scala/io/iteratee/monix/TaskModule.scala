package io.iteratee.monix

import cats.effect.Sync
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.modules.FileModule
import monix.eval.Task

trait TaskModule extends Module[Task]
    with EnumerateeModule[Task]
    with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable]
    with FileModule[Task] {
      type M[f[_]] = Sync[f]
    }

final object TaskModule {
  def instance(implicit taskSync: Sync[Task]): TaskModule = new TaskModule {
    final protected val F: Sync[Task] = taskSync
  }
}
