package io.travisbrown.iteratee

import cats.Monad
import cats.laws.discipline.ArbitraryK
import org.scalacheck.{ Arbitrary, Gen }

trait ArbitraryInstances {
  implicit def arbitraryInput[A](implicit A: Arbitrary[A]): Arbitrary[Input[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Input.emptyInput[A]),
        A.arbitrary.map(Input.elInput[A](_)),
        Gen.const(Input.eofInput[A])
      )
    )

  implicit def arbitraryEnumeratorT[A, F[_]: Monad](implicit
    A: Arbitrary[A]
  ): Arbitrary[EnumeratorT[A, F]] =
    Arbitrary(
      Gen.containerOfN[List, A](10, A.arbitrary).map(EnumeratorT.enumList[A, F])
    )
}

trait ArbitraryKInstances extends ArbitraryInstances {
  implicit def arbitraryKInput: ArbitraryK[Input] =
    new ArbitraryK[Input] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Input[A]] = arbitraryInput[A]
    }

  implicit def arbitraryKEnumeratorT[F[_]: Monad]: ArbitraryK[EnumeratorT[?, F]] =
    new ArbitraryK[EnumeratorT[?, F]] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[EnumeratorT[A, F]] =
        arbitraryEnumeratorT[A, F]
    }
}
