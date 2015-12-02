package io.iteratee

import cats.Monad
import org.scalacheck.{ Arbitrary, Gen }

trait ArbitraryInstances {
  implicit def arbitraryInput[A](implicit A: Arbitrary[A]): Arbitrary[Input[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Input.empty[A]),
        A.arbitrary.map(Input.el),
        Arbitrary.arbitrary[Vector[A]].map(Input.chunk),
        Gen.const(Input.end[A])
      )
    )

  implicit def arbitraryEnumerator[A, F[_]: Monad](implicit
    A: Arbitrary[A]
  ): Arbitrary[Enumerator[A, F]] =
    Arbitrary(
      Gen.containerOfN[List, A](10, A.arbitrary).flatMap(as =>
        Gen.oneOf(
          Enumerator.enumList[A, F](as),
          Enumerator.enumStream[A, F](as.toStream)
        )
      )
    )

  implicit def arbitraryIteratee[A, F[_]: Monad](implicit
    A: Arbitrary[A]
  ): Arbitrary[Iteratee[Vector[A], F, Vector[A]]] = {
    val m: Monad[({ type L[x] = Iteratee[Vector[A], F, x] })#L] = implicitly
    val f = Iteratee.fold[Vector[A], F, Vector[A]](Vector.empty)(_ ++ _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        xs <- Gen.containerOfN[Vector, A](128, A.arbitrary)
        it <- Gen.oneOf(
          Iteratee.drop[Vector[A], F](n).flatMap(_ => f),
          Iteratee.drop[Vector[A], F](n).flatMap(_ => m.pure(xs)),
          Iteratee.peek[Vector[A], F].flatMap(_ => f),
          Iteratee.peek[Vector[A], F].flatMap(_ => m.pure(xs)),
          Iteratee.identity[Vector[A], F].flatMap(_ => f),
          Iteratee.identity[Vector[A], F].flatMap(_ => m.pure(xs))
        )
      } yield it
    )
  }
}
