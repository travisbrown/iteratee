package io.iteratee

import cats.{ Eval, MonadError }
import cats.data.EitherT
import cats.instances.either.catsStdInstancesForEither
import io.iteratee.modules.{ EitherModule, EitherTModule, FutureModule, TryModule }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

package object files {
  def future(implicit ec0: ExecutionContext): FutureModule with FileModule[Future] = new FutureFileModule {
    final protected def ec: ExecutionContext = ec0
  }
}

package files {
  final object either extends EitherFileModule
  final object eitherT extends EitherTFileModule
  final object try_ extends TryFileModule

  trait EitherFileModule extends EitherModule with NonSuspendableFileModule[Either[Throwable, ?]]

  trait EitherTFileModule extends EitherTModule with SuspendableFileModule[EitherT[Eval, Throwable, ?]] {
    private[this] val E: MonadError[Either[Throwable, ?], Throwable] = catsStdInstancesForEither[Throwable]
    final protected def captureEffect[A](a: => A): EitherT[Eval, Throwable, A] =
      EitherT(Eval.always(E.catchNonFatal(a)))
  }

  trait FutureFileModule extends FutureModule with NonSuspendableFileModule[Future]
  trait TryFileModule extends TryModule with NonSuspendableFileModule[Try]
}
