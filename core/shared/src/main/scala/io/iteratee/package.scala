package io

import cats.{ Eval, Id }
import cats.data.XorT

package object iteratee {
  private[iteratee] final def diverge[A]: A = sys.error("Divergent iteratee")
}

package iteratee {
  final object pure extends Module[Id]
  final object eval extends Module[Eval]
  final object option extends Module[Option]
  final object xor extends Module[({ type L[x] = XorT[Eval, Throwable, x] })#L]
}
