package io.travisbrown.iteratee

import cats.Monad
import cats.laws.discipline.ArbitraryK
import org.scalacheck.{ Arbitrary, Gen }

trait ArbitraryInstances {
  implicit def arbitraryInput[A](implicit A: Arbitrary[A]): Arbitrary[Input[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Input.empty[A]),
        A.arbitrary.map(Input.el),
        Arbitrary.arbitrary[Vector[A]].map(Input.chunk),
        Gen.const(Input.eof[A])
      )
    )

  implicit def arbitraryEnumerator[A, F[_]: Monad](implicit
    A: Arbitrary[A]
  ): Arbitrary[Enumerator[A, F]] =
    Arbitrary(
      Gen.containerOfN[List, A](10, A.arbitrary).map(Enumerator.enumList[A, F])
    )
}

trait ArbitraryKInstances extends ArbitraryInstances {
  implicit def arbitraryKInput: ArbitraryK[Input] =
    new ArbitraryK[Input] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Input[A]] = arbitraryInput[A]
    }

  implicit def arbitraryKEnumerator[F[_]: Monad]: ArbitraryK[({ type L[x] = Enumerator[x, F] })#L] =
    new ArbitraryK[({ type L[x] = Enumerator[x, F] })#L] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Enumerator[A, F]] =
        arbitraryEnumerator[A, F]
    }
}
