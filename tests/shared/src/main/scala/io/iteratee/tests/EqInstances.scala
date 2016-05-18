package io.iteratee.tests

import cats.{ Eq, Monad }
import io.iteratee.{ Enumeratee, Enumerator, Iteratee }
import org.scalacheck.Arbitrary

trait EqInstances {
  implicit val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit def eqEnumerator[F[_]: Monad, A: Eq](implicit eq: Eq[F[Vector[A]]]): Eq[Enumerator[F, A]] =
    eq.on[Enumerator[F, A]](_.toVector)

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
      eq.eqv(e0.mapE(i).toVector, e0.mapE(j).toVector) &&
      eq.eqv(e1.mapE(i).toVector, e1.mapE(j).toVector) &&
      eq.eqv(e2.mapE(i).toVector, e2.mapE(j).toVector) &&
      eq.eqv(e3.mapE(i).toVector, e3.mapE(j).toVector)
    }
  }
}
