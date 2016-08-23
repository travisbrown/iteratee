package io.iteratee.files

import cats.Eval
import cats.data.{ Xor, XorT }
import io.iteratee.XorModule

trait XorFileModule extends XorModule with FileModule[({ type L[x] = XorT[Eval, Throwable, x] })#L] {
  final protected def effectCapture: EffectCapture[({ type L[x] = XorT[Eval, Throwable, x] })#L] =
    new EffectCapture[({ type L[x] = XorT[Eval, Throwable, x] })#L] {
      def apply[A](a: => A): XorT[Eval, Throwable, A] = XorT(Eval.always(Xor.catchNonFatal(a)))
    }
}
