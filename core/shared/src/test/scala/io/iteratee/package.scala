package io

import algebra.Eq
import org.scalatest.Tag

package object iteratee {
  implicit def eqTuple2[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] =
    Eq.instance {
      case ((a1, a2), (b1, b2)) => A.eqv(a1, b1) && B.eqv(a2, b2)
    }

  implicit def eqTuple3[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    Eq.instance {
      case ((a1, b1, c1), (a2, b2, c2)) => A.eqv(a1, a2) && B.eqv(b1, b2) && C.eqv(c1, c2)
    }
}

package iteratee {
  object NoScala210Test extends Tag("io.iteratee.NoScala210Test")
}
