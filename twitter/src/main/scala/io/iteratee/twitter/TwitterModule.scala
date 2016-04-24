package io.iteratee.twitter

import cats.MonadError
import com.twitter.util.Future
import io.catbird.util.Rerunnable
import io.iteratee.Module
import io.iteratee.files.FileModule

trait TwitterModule extends Module[Rerunnable] with FileModule[Rerunnable] {
  final protected val fileModuleF: MonadError[Rerunnable, Throwable] = implicitly

  final protected def captureEffect[A](a: => A): Rerunnable[A] = new Rerunnable[A] {
    final def run: Future[A] = Future(a)
  }
}
