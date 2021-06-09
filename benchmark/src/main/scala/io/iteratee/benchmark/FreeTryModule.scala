package io.iteratee.benchmark

import cats.{Monad, MonadError}
import cats.effect.Sync
import cats.free.Free
import cats.instances.try_._
import io.iteratee.files.modules.FileModule
import io.iteratee.modules.{EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module}
import scala.util.Try

object FreeTryModule
    extends Module[Free[Try, *]]
    with EnumerateeModule[Free[Try, *]]
    with EnumeratorErrorModule[Free[Try, *], Throwable]
    with IterateeErrorModule[Free[Try, *], Throwable]
    with FileModule[Free[Try, *]] {
  final type M[f[_]] = Sync[f]

  final protected val F: Sync[Free[Try, *]] = new Sync[Free[Try, *]] {
    private[this] val FF = Monad[Free[Try, *]]
    def pure[A](x: A): Free[Try, A] = FF.pure(x)
    def raiseError[A](e: Throwable): Free[Try, A] = Free.liftF(MonadError[Try, Throwable].raiseError(e))
    def handleErrorWith[A](fa: Free[Try, A])(f: Throwable => Free[Try, A]): Free[Try, A] = Free.liftF(
      MonadError[Try, Throwable].handleErrorWith(fa.runTailRec)(e => f(e).runTailRec)
    )
    def flatMap[A, B](fa: Free[Try, A])(f: A => Free[Try, B]): Free[Try, B] = FF.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => Free[Try, Either[A, B]]): Free[Try, B] = FF.tailRecM(a)(f)

    def suspend[A](hint: Sync.Type)(thunk: => A): Free[Try, A] = Free.defer(Free.pure(thunk))

    def monotonic: Free[Try, scala.concurrent.duration.FiniteDuration] = Predef.???
    def realTime: Free[Try, scala.concurrent.duration.FiniteDuration] = Predef.???
    def canceled: Free[Try, Unit] = Predef.???
    def forceR[A, B](fa: Free[Try, A])(fb: Free[Try, B]): Free[Try, B] = Predef.???
    def onCancel[A](fa: Free[Try, A], fin: Free[Try, Unit]): Free[Try, A] = Predef.???
    def rootCancelScope: cats.effect.kernel.CancelScope = Predef.???
    def uncancelable[A](body: cats.effect.kernel.Poll[Free[Try, *]] => Free[Try, A]): Free[Try, A] = Predef.???
  }
}
