package io.iteratee.benchmark

import cats.{ Monad, MonadError }
import cats.free.Free
import com.twitter.util.Try
import io.catbird.util.twitterTryInstance
import io.iteratee.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import io.iteratee.files.SuspendableFileModule

object FreeTryModule extends Module[Free[Try, ?]]
    with EnumerateeModule[Free[Try, ?]]
    with EnumeratorErrorModule[Free[Try, ?], Throwable]
    with IterateeErrorModule[Free[Try, ?], Throwable]
    with SuspendableFileModule[Free[Try, ?]] {
  final type M[f[_]] = MonadError[f, Throwable]

  def captureEffect[A](a: => A): Free[Try, A] = Free.defer(Free.liftF(MonadError[Try, Throwable].catchNonFatal(a)))

  final protected val F: MonadError[Free[Try, ?], Throwable] = new MonadError[Free[Try, ?], Throwable] {
    private[this] val FF = Monad[Free[Try, ?]]
    def pure[A](x: A): Free[Try, A] = FF.pure(x)
    def raiseError[A](e: Throwable): Free[Try, A] = Free.liftF(MonadError[Try, Throwable].raiseError(e))
    def handleErrorWith[A](fa: Free[Try, A])(f: Throwable => Free[Try, A]): Free[Try, A] = Free.liftF(
      MonadError[Try, Throwable].handleErrorWith(fa.runTailRec)(e => f(e).runTailRec)
    )
    def flatMap[A, B](fa: Free[Try, A])(f: A => Free[Try, B]): Free[Try, B] = FF.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => Free[Try, Either[A, B]]): Free[Try, B] = FF.tailRecM(a)(f)
  }
}
