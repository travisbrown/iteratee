package io.iteratee

import algebra.Eq
import cats.Monad
import org.scalacheck.Arbitrary

trait EqInstances {
  implicit def eqEnumerator[A: Eq, F[_]: Monad](implicit
    eq: Eq[F[Vector[A]]]
  ): Eq[Enumerator[A, F]] = eq.on[Enumerator[A, F]](_.drain)

  implicit def eqIteratee[A: Eq: Arbitrary, F[_]: Monad](implicit
    eq: Eq[F[Vector[A]]]
  ): Eq[Iteratee[Vector[A], F, Vector[A]]] = {
    val e0 = Enumerator.empty[Vector[A], F]
    val e1 = Enumerator.enumList[Vector[A], F](Arbitrary.arbitrary[List[Vector[A]]].sample.get)
    val e2 = Enumerator.enumStream[Vector[A], F](Arbitrary.arbitrary[Stream[Vector[A]]].sample.get)

    Eq.instance { (i, j) =>
      eq.eqv(i.process(e0), j.process(e0)) &&
      eq.eqv(i.process(e1), j.process(e1)) &&
      eq.eqv(i.process(e2), j.process(e2))
    }
  }
}
