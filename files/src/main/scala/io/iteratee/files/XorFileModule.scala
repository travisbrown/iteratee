package io.iteratee.files

import cats.Eval
import cats.data.{ Xor, XorT }
import io.iteratee.XorModule

trait XorFileModule extends XorModule with FileModule[({ type L[x] = XorT[Eval, Throwable, x] })#L] {
  final override protected def captureEffect[A](a: => A): Eval[XorT[Eval, Throwable, A]] =
    // XorT with Eval here is already lazy, so the outer Eval can be strict
    Eval.now(XorT(Eval.always(Xor.catchNonFatal(a))))
}
