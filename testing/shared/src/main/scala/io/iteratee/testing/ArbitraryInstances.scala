package io.iteratee.testing

import cats.Monad
import cats.instances.int._
import io.iteratee.{ Iteratee, Enumeratee, Enumerator }
import org.scalacheck.{ Arbitrary, Gen }
import scala.Predef._

trait ArbitraryInstances {
  implicit def arbitraryEnumerator[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Enumerator[F, A]] =
    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        as <- Gen.containerOfN[List, A](n, A.arbitrary)
        en <- Gen.oneOf(
          Enumerator.enumList[F, A](as),
          Enumerator.enumStream[F, A](as.toStream)
        )
      } yield en
    )

  implicit def arbitraryIntIteratee[F[_]: Monad]: Arbitrary[Iteratee[F, Int, Int]] = {
    val M: Monad[({ type L[x] = Iteratee[F, Int, x] })#L] = implicitly
    val F = Iteratee.fold[F, Int, Int](0)(_ + _)

    Arbitrary(
      for {
        n <- Gen.chooseNum(0, 16)
        a <- Arbitrary.arbitrary[Int]
        r <- Arbitrary.arbitrary[Vector[Int]]
        it <- Gen.oneOf[Iteratee[F, Int, Int]](
          Iteratee.done[F, Int, Int](a),
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
        asSize <- Gen.chooseNum(0, 128)
        as <- Gen.containerOfN[Vector, A](asSize, A.arbitrary)
        rSize <- Gen.chooseNum(0, 128)
        r <- Gen.containerOfN[Vector, Vector[A]](rSize, Arbitrary.arbitrary[Vector[A]])
        it <- Gen.oneOf[Iteratee[F, Vector[A], Vector[A]]](
          Iteratee.done[F, Vector[A], Vector[A]](as),
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

  implicit def arbitraryVectorUnitIteratee[F[_]: Monad, A](implicit
    A: Arbitrary[A]
  ): Arbitrary[Iteratee[F, Vector[A], Unit]] = Arbitrary(
    arbitraryVectorIteratee[F, A].arbitrary.map(_.discard)
  )

  implicit def arbitraryFunctionIteratee[F[_]: Monad, A]: Arbitrary[Iteratee[F, A, Vector[Int] => Vector[Int]]] = {
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

  implicit def arbitraryEnumeratee[F[_]: Monad]: Arbitrary[Enumeratee[F, Int, Int]] =
    Arbitrary(
      for {
        a <- Arbitrary.arbitrary[Int]
        f <- Arbitrary.arbitrary[Int => Int]
        en <- arbitraryEnumerator[F, Int].arbitrary
        et <- Gen.oneOf[Enumeratee[F, Int, Int]](
          Enumeratee.map[F, Int, Int](f),
          Enumeratee.map[F, Int, Int](_ + 1),
          Enumeratee.map[F, Int, Int](_ => a),
          Enumeratee.flatMap[F, Int, Int](_ => en),
          Enumeratee.filter[F, Int](_ % 2 == 0),
          Enumeratee.collect[F, Int, Int] {
            case i if i.toString.last != '0' => i
          },
          Enumeratee.sequenceI[F, Int, Int](Iteratee.take(2).map(_.head)),
          Enumeratee.uniq[F, Int]
        )
      } yield et
    )
}
