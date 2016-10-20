package io.iteratee

import cats.{ Eval, MonadError }
import cats.data.EitherT
import cats.instances.either.catsStdInstancesForEither
import io.iteratee.modules.{ EitherModule, EitherTModule, FutureModule, TryModule }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

package object files {
  def future(implicit ec0: ExecutionContext): FileModule[Future] = new FutureFileModule {
    final protected val ec: ExecutionContext = ec0
  }
}

package files {
  final object either extends EitherFileModule
  final object eitherT extends EitherTFileModule
  final object try_ extends TryFileModule

  trait EitherFileModule extends EitherModule with NonSuspendableFileModule[({ type L[x] = Either[Throwable, x] })#L]

  trait EitherTFileModule extends EitherTModule
      with SuspendableFileModule[({ type L[x] = EitherT[Eval, Throwable, x] })#L] {
    private[this] val E: MonadError[({ type L[x] = Either[Throwable, x] })#L, Throwable] =
     catsStdInstancesForEither[Throwable]
    final protected def captureEffect[A](a: => A): EitherT[Eval, Throwable, A] =
      EitherT(Eval.always(E.catchNonFatal(a)))
  }

  trait FutureFileModule extends FutureModule with NonSuspendableFileModule[Future]
  trait TryFileModule extends TryModule with NonSuspendableFileModule[Try]
}
