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

  implicit def arbitraryEnumerator[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Enumerator[F, A]] =
    Arbitrary(
      Gen.containerOfN[List, A](10, A.arbitrary).flatMap(as =>
        Gen.oneOf(
          Enumerator.enumList[F, A](as),
          Enumerator.enumStream[F, A](as.toStream)
        )
      )
    )

  implicit def arbitraryIteratee[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Iteratee[F, Vector[A], Vector[A]]] = {
    val m: Monad[({ type L[x] = Iteratee[F, Vector[A], x] })#L] = implicitly
    val f = Iteratee.fold[F, Vector[A], Vector[A]](Vector.empty)(_ ++ _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        xs <- Gen.containerOfN[Vector, A](128, A.arbitrary)
        it <- Gen.oneOf(
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => f),
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => m.pure(xs)),
          Iteratee.peek[F, Vector[A]].flatMap(_ => f),
          Iteratee.peek[F, Vector[A]].flatMap(_ => m.pure(xs)),
          Iteratee.identity[F, Vector[A]].flatMap(_ => f),
          Iteratee.identity[F, Vector[A]].flatMap(_ => m.pure(xs))
        )
      } yield it
    )
  }
}
