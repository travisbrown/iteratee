package io.iteratee

import algebra.Eq
import cats.Monad
import cats.laws.discipline.EqK
import cats.std.vector._
import org.scalacheck.Arbitrary

trait EqInstances {
  implicit def eqEnumerator[A: Eq, F[_]: Monad](implicit F: EqK[F]): Eq[Enumerator[A, F]] =
    F.synthesize[Vector[A]].on[Enumerator[A, F]](_.drainTo)

  implicit def eqIteratee[A: Eq: Arbitrary, F[_]: Monad](implicit
    F: EqK[F]
  ): Eq[Iteratee[Vector[A], F, Vector[A]]] = {
    val eq = F.synthesize[Vector[A]]
    val e0 = Enumerator.empty[Vector[A], F]
    val e1 = Enumerator.enumList[Vector[A], F](Arbitrary.arbitrary[List[Vector[A]]].sample.get)
    val e2 = Enumerator.enumStream[Vector[A], F](Arbitrary.arbitrary[Stream[Vector[A]]].sample.get)

    Eq.instance { (i, j) =>
      eq.eqv(i.feedE(e0).run , j.feedE(e0).run) &&
      eq.eqv(i.feedE(e1).run , j.feedE(e1).run) &&
      eq.eqv(i.feedE(e2).run , j.feedE(e2).run)
    }
  }
}
