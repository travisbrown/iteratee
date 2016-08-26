package io.iteratee

import cats.{ Eval, Id, Monad, MonadError, catsInstancesForId }
import cats.data.{ EitherT, XorT }
import cats.instances.either.catsStdInstancesForEither
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.option.catsStdInstancesForOption
import cats.instances.try_.catsStdInstancesForTry
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

package object modules {
  def future(implicit ec0: ExecutionContext): FutureModule = new FutureModule {
    final protected def ec: ExecutionContext = ec0
  }
}

package modules {
  final object either extends EitherModule
  final object eitherT extends EitherTModule
  final object eval extends EvalModule
  final object id extends IdModule
  final object option extends OptionModule
  final object try_ extends TryModule

  @deprecated("Use eitherT", "0.6.0")
  final object xor extends XorModule

  trait EitherModule extends Module[({ type L[x] = Either[Throwable, x] })#L]
      with EnumerateeModule[({ type L[x] = Either[Throwable, x] })#L]
      with EnumeratorErrorModule[({ type L[x] = Either[Throwable, x] })#L, Throwable]
      with IterateeErrorModule[({ type L[x] = Either[Throwable, x] })#L, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    final protected val F: MonadError[({ type L[x] = Either[Throwable, x] })#L, Throwable] = catsStdInstancesForEither
  }

  trait EitherTModule extends Module[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
      with EnumerateeModule[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
      with EnumeratorErrorModule[({ type L[x] = EitherT[Eval, Throwable, x] })#L, Throwable]
      with IterateeErrorModule[({ type L[x] = EitherT[Eval, Throwable, x] })#L, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    final protected val F: MonadError[({ type L[x] = EitherT[Eval, Throwable, x] })#L, Throwable] =
      EitherT.catsDataMonadErrorForEitherT
  }

  trait EvalModule extends Module[Eval]
      with EnumerateeModule[Eval] with EnumeratorModule[Eval] with IterateeModule[Eval] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Eval] = Eval.catsBimonadForEval
  }

  trait FutureModule extends Module[Future]
      with EnumerateeModule[Future]
      with EnumeratorErrorModule[Future, Throwable]
      with IterateeErrorModule[Future, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    protected def ec: ExecutionContext
    final protected val F: MonadError[Future, Throwable] = catsStdInstancesForFuture(ec)
  }

  trait IdModule extends Module[Id]
      with EnumerateeModule[Id] with EnumeratorModule[Id] with IterateeModule[Id] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Id] = catsInstancesForId
  }

  trait OptionModule extends Module[Option]
      with EnumerateeModule[Option] with EnumeratorModule[Option] with IterateeModule[Option] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Option] = catsStdInstancesForOption
  }

  trait TryModule extends Module[Try]
      with EnumerateeModule[Try]
      with EnumeratorErrorModule[Try, Throwable]
      with IterateeErrorModule[Try, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    final protected val F: MonadError[Try, Throwable] = catsStdInstancesForTry
  }

  @deprecated("Use EitherTModule", "0.6.0")
  trait XorModule extends Module[({ type L[x] = XorT[Eval, Throwable, x] })#L]
      with EnumerateeModule[({ type L[x] = XorT[Eval, Throwable, x] })#L]
      with EnumeratorErrorModule[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable]
      with IterateeErrorModule[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    final protected val F: MonadError[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable] =
      XorT.catsDataMonadErrorForXorT
  }
}
