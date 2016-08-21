package io.iteratee.files

import cats.{ Eq, Eval }
import cats.data.{ Xor, XorT }
import io.iteratee.tests.files.FileModuleSuite

class XorFileModuleSuite extends FileModuleSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorFileModule {
  def monadName: String = "XorT[Eval, Throwable, ?]"

  implicit def eqEval[A](implicit A: Eq[A]): Eq[Eval[Xor[Throwable, A]]] =
    Eval.catsEqForEval(Xor.catsDataEqForXor(Eq.fromUniversalEquals, A))

  implicit def eqF[A](implicit A: Eq[A]): Eq[XorT[Eval, Throwable, A]] = XorT.catsDataEqForXorT(eqEval(A))
}
