package io.iteratee.twitter

import cats.MonadError
import com.twitter.util.Future
import io.catbird.util.Rerunnable
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.FileModule

trait TwitterModule extends Module[Rerunnable]
  with EnumerateeModule[Rerunnable]
  with EnumeratorErrorModule[Rerunnable, Throwable] with IterateeErrorModule[Rerunnable, Throwable]
  with FileModule[Rerunnable] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Rerunnable, Throwable] = implicitly

  final protected def captureEffect[A](a: => A): Rerunnable[A] = new Rerunnable[A] {
    final def run: Future[A] = Future(a)
  }
}
