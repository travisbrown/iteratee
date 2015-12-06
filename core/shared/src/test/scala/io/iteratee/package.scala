package io

import algebra.Eq

package object iteratee {
  implicit def eqTuple2[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] =
    Eq.instance {
      case ((a1, a2), (b1, b2)) => A.eqv(a1, b1) && B.eqv(a2, b2)
    }
}
