package io.iteratee

import algebra.Eq
import cats.Monad
import org.scalacheck.Arbitrary

trait EqInstances {
  implicit def eqEnumerator[F[_]: Monad, A: Eq](implicit
    eq: Eq[F[Vector[A]]]
  ): Eq[Enumerator[F, A]] = eq.on[Enumerator[F, A]](_.drain)

  implicit def eqIteratee[F[_]: Monad, A: Eq: Arbitrary](implicit
    eq: Eq[F[Vector[A]]]
  ): Eq[Iteratee[F, Vector[A], Vector[A]]] = {
    val e0 = Enumerator.empty[F, Vector[A]]
    val e1 = Enumerator.enumList[F, Vector[A]](Arbitrary.arbitrary[List[Vector[A]]].sample.get)
    val e2 = Enumerator.enumStream[F, Vector[A]](Arbitrary.arbitrary[Stream[Vector[A]]].sample.get)
    val e3 = Enumerator.enumVector[F, Vector[A]](Arbitrary.arbitrary[Vector[Vector[A]]].sample.get)

    Eq.instance { (i, j) =>
      eq.eqv(i.process(e0), j.process(e0)) &&
      eq.eqv(i.process(e1), j.process(e1)) &&
      eq.eqv(i.process(e2), j.process(e2)) &&
      eq.eqv(i.process(e3), j.process(e3))
    }
  }
}
