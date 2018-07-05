package io.iteratee.benchmark

import cats.{ Monad, MonadError }
import cats.effect.{ ExitCase, Sync }
import cats.free.Free
import cats.instances.try_._
import io.iteratee.files.modules.FileModule
import io.iteratee.modules.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }
import scala.util.{ Failure, Success, Try }

object FreeTryModule extends Module[Free[Try, ?]]
    with EnumerateeModule[Free[Try, ?]]
    with EnumeratorErrorModule[Free[Try, ?], Throwable]
    with IterateeErrorModule[Free[Try, ?], Throwable]
    with FileModule[Free[Try, ?]] {
  final type M[f[_]] = Sync[f]

  final protected val F: Sync[Free[Try, ?]] = new Sync[Free[Try, ?]] {
    private[this] val FF = Monad[Free[Try, ?]]
    def pure[A](x: A): Free[Try, A] = FF.pure(x)
    def raiseError[A](e: Throwable): Free[Try, A] = Free.liftF(MonadError[Try, Throwable].raiseError(e))
    def handleErrorWith[A](fa: Free[Try, A])(f: Throwable => Free[Try, A]): Free[Try, A] = Free.liftF(
      MonadError[Try, Throwable].handleErrorWith(fa.runTailRec)(e => f(e).runTailRec)
    )
    def flatMap[A, B](fa: Free[Try, A])(f: A => Free[Try, B]): Free[Try, B] = FF.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => Free[Try, Either[A, B]]): Free[Try, B] = FF.tailRecM(a)(f)

    // Don't use this; it's a quick hack for benchmarking only.
    final def bracketCase[A, B](acquire: Free[Try, A])(use: A => Free[Try, B])(
      release: (A, ExitCase[Throwable]) => Free[Try, Unit]
    ): Free[Try, B] = acquire.flatMap { a =>
      val result = use(a)

      result.fold[Free[Try, B]](
        b => release(a, ExitCase.complete).map(_ => b),
        {
          case Success(f) => release(a, ExitCase.complete).flatMap(_ => f)
          case Failure(e) => release(a, ExitCase.error(e)).flatMap(_ => result)
        }
      )
    }

    def suspend[A](thunk: => Free[Try, A]): Free[Try, A] = Free.defer(thunk)
  }
}
