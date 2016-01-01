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
        A.arbitrary.map(Input.el),
        for {
          a1 <- A.arbitrary
          a2 <- A.arbitrary
          as <- Arbitrary.arbitrary[Vector[A]]
        } yield Input.chunk(a1, a1, as),
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

  implicit def arbitraryIntIteratee[F[_]: Monad, A]: Arbitrary[Iteratee[F, Int, Int]] = {
    val M: Monad[({ type L[x] = Iteratee[F, Int, x] })#L] = implicitly
    val F = Iteratee.fold[F, Int, Int](0)(_ + _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        a <- Arbitrary.arbitrary[Int]
        it <- Gen.oneOf[Iteratee[F, Int, Int]](
          Iteratee.drop[F, Int](n).flatMap(_ => F),
          Iteratee.drop[F, Int](n).flatMap(_ => M.pure(a)),
          Iteratee.head[F, Int].map(_.getOrElse(0)),
          Iteratee.peek[F, Int].flatMap(_ => F),
          Iteratee.peek[F, Int].flatMap(head => M.pure(a + head.getOrElse(0))),
          Iteratee.take[F, Int](n).flatMap(taken => M.pure(taken.sum)),
          Iteratee.identity[F, Int].flatMap(_ => F),
          Iteratee.identity[F, Int].flatMap(_ => M.pure(a))
        )
      } yield it
    )
  }

  implicit def arbitraryVectorIteratee[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Iteratee[F, Vector[A], Vector[A]]] = {
    val M: Monad[({ type L[x] = Iteratee[F, Vector[A], x] })#L] = implicitly
    val F = Iteratee.fold[F, Vector[A], Vector[A]](Vector.empty)(_ ++ _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        as <- Gen.containerOfN[Vector, A](128, A.arbitrary)
        it <- Gen.oneOf[Iteratee[F, Vector[A], Vector[A]]](
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => F),
          Iteratee.drop[F, Vector[A]](n).flatMap(_ => M.pure(as)),
          Iteratee.head[F, Vector[A]].map(_.getOrElse(Vector.empty)),
          Iteratee.peek[F, Vector[A]].flatMap(_ => F),
          Iteratee.peek[F, Vector[A]].flatMap(head =>
            M.pure(as ++ head.fold(Vector.empty[A])(_.take(n)))
          ),
          Iteratee.take[F, Vector[A]](n).flatMap(taken => M.pure(taken.flatMap(_.headOption))),
          Iteratee.identity[F, Vector[A]].flatMap(_ => F),
          Iteratee.identity[F, Vector[A]].flatMap(_ => M.pure(as))
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
        (as: Vector[Int]) => Vector(as.size),
        (as: Vector[Int]) => as,
        (as: Vector[Int]) => as.drop(2),
        (as: Vector[Int]) => as.map(_ * 2)
      ).map(M.pure(_))
    )
  }
}
