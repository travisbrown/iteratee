package io.iteratee

import cats.Monad
import cats.data.Xor
import io.iteratee.internal.Input
import org.scalacheck.{ Arbitrary, Gen }

trait ArbitraryInstances {
  implicit def arbitraryXor[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Xor[A, B]] =
    Arbitrary(
      Arbitrary.arbitrary[Either[A, B]].map(Xor.fromEither)
    )

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

  implicit def arbitraryVectorIteratee[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Iteratee[F, Vector[A], Vector[A]]] = {
    val M: Monad[({ type L[x] = Iteratee[F, Vector[A], x] })#L] = implicitly
    val F = Iteratee.fold[F, Vector[A], Vector[A]](Vector.empty)(_ ++ _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        xs <- Gen.containerOfN[Vector, A](128, A.arbitrary)
        it <- Gen.oneOf(
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => F),
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => M.pure(xs)),
          Iteratee.peek[F, Vector[A]].flatMap(_ => F),
          Iteratee.peek[F, Vector[A]].flatMap(_ => M.pure(xs)),
          Iteratee.identity[F, Vector[A]].flatMap(_ => F),
          Iteratee.identity[F, Vector[A]].flatMap(_ => M.pure(xs))
        )
      } yield it
    )
  }

  implicit def arbitraryFunctionIteratee[
    F[_]: Monad,
    A
  ]: Arbitrary[Iteratee[F, A, Vector[Int] => Vector[Int]]] = {
    val M: Monad[({ type L[x] = Iteratee[F, A, x] })#L] = implicitly

    Arbitrary(
      Gen.oneOf(
        (xs: Vector[Int]) => Vector(xs.size),
        (xs: Vector[Int]) => xs,
        (xs: Vector[Int]) => xs.drop(2),
        (xs: Vector[Int]) => xs.map(_ * 2)
      ).map(M.pure(_))
    )
  }
}
