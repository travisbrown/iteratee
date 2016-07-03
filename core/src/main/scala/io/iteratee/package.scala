package io

import cats.{ Monad, MonadError, Eval, Id }
import cats.data.XorT
import scala.Predef.implicitly

package object iteratee

package iteratee {
  final object pure extends PureModule
  final object eval extends EvalModule
  final object option extends OptionModule
  final object xor extends XorModule

  trait PureModule extends Module[Id]
    with EnumerateeModule[Id] with EnumeratorModule[Id] with IterateeModule[Id] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Id] = implicitly
  }


  trait EvalModule extends Module[Eval]
    with EnumerateeModule[Eval] with EnumeratorModule[Eval] with IterateeModule[Eval] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Eval] = implicitly
  }

  trait OptionModule extends Module[Option]
    with EnumerateeModule[Option] with EnumeratorModule[Option] with IterateeModule[Option] {
    final type M[f[_]] = Monad[f]

    final protected val F: Monad[Option] = cats.std.option.optionInstance
  }

  trait XorModule extends Module[({ type L[x] = XorT[Eval, Throwable, x] })#L]
    with EnumerateeModule[({ type L[x] = XorT[Eval, Throwable, x] })#L]
    with EnumeratorErrorModule[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable]
    with IterateeErrorModule[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable] {
    final type M[f[_]] = MonadError[f, Throwable]

    final protected val F: MonadError[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable] = implicitly
  }
}
