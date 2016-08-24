package io.iteratee.twitter

import cats.MonadError
import io.catbird.util.Rerunnable
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.SuspendableFileModule

trait RerunnableModule extends Module[Rerunnable]
    with EnumerateeModule[Rerunnable]
    with EnumeratorErrorModule[Rerunnable, Throwable]
    with IterateeErrorModule[Rerunnable, Throwable]
    with SuspendableFileModule[Rerunnable] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Rerunnable, Throwable] = Rerunnable.rerunnableInstance
  final override protected def captureEffect[A](a: => A): Rerunnable[A] = Rerunnable(a)
}
