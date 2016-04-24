package io.iteratee.files

import cats.{ Eval, MonadError }
import cats.data.{ Xor, XorT }

trait XorFileModule extends FileModule[({ type L[x] = XorT[Eval, Throwable, x] })#L] {
  final protected val fileModuleF: MonadError[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable] = implicitly

  final protected def captureEffect[A](a: => A): XorT[Eval, Throwable, A] = XorT(Eval.always(Xor.catchNonFatal(a)))
}
