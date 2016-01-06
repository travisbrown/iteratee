package io.iteratee.tests

import algebra.Eq
import cats.Monad
import io.iteratee.{ Enumeratee, Enumerator, Iteratee }
import org.scalacheck.Arbitrary

trait EqInstances {
  implicit def eqTuple2[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] =
    Eq.instance {
      case ((a1, a2), (b1, b2)) => A.eqv(a1, b1) && B.eqv(a2, b2)
    }

  implicit def eqTuple3[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    Eq.instance {
      case ((a1, b1, c1), (a2, b2, c2)) => A.eqv(a1, a2) && B.eqv(b1, b2) && C.eqv(c1, c2)
    }

  implicit val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit def eqEnumerator[F[_]: Monad, A: Eq](implicit
    eq: Eq[F[Vector[A]]]
  ): Eq[Enumerator[F, A]] = eq.on[Enumerator[F, A]](_.drain)

  implicit def eqIteratee[F[_]: Monad, A: Eq: Arbitrary, B: Eq: Arbitrary](implicit
    eq: Eq[F[B]]
  ): Eq[Iteratee[F, A, B]] = {
    val e0 = Enumerator.empty[F, A]
    val e1 = Enumerator.enumList[F, A](Arbitrary.arbitrary[List[A]].sample.get)
    val e2 = Enumerator.enumStream[F, A](Arbitrary.arbitrary[Stream[A]].sample.get)
    val e3 = Enumerator.enumVector[F, A](Arbitrary.arbitrary[Vector[A]].sample.get)

    Eq.instance { (i, j) =>
      eq.eqv(e0.run(i), e0.run(j)) &&
      eq.eqv(e1.run(i), e1.run(j)) &&
      eq.eqv(e2.run(i), e2.run(j)) &&
      eq.eqv(e3.run(i), e3.run(j))
    }
  }

  implicit def eqEnumeratee[F[_]: Monad, A: Eq: Arbitrary, B: Eq: Arbitrary](implicit
    eq: Eq[F[Vector[B]]]
  ): Eq[Enumeratee[F, A, B]] = {
    val e0 = Enumerator.empty[F, A]
    val e1 = Enumerator.enumList[F, A](Arbitrary.arbitrary[List[A]].sample.get)
    val e2 = Enumerator.enumStream[F, A](Arbitrary.arbitrary[Stream[A]].sample.get)
    val e3 = Enumerator.enumVector[F, A](Arbitrary.arbitrary[Vector[A]].sample.get)

    Eq.instance { (i, j) =>
      eq.eqv(e0.mapE(i).drain, e0.mapE(j).drain) &&
      eq.eqv(e1.mapE(i).drain, e1.mapE(j).drain) &&
      eq.eqv(e2.mapE(i).drain, e2.mapE(j).drain) &&
      eq.eqv(e3.mapE(i).drain, e3.mapE(j).drain)
    }
  }
}
