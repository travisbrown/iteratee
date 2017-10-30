package io.iteratee.testing

import io.iteratee.{ Enumerator, EnumeratorModule, Iteratee, IterateeModule, Module }
import org.scalacheck.{ Arbitrary, Gen }

trait ArbitraryEnumerators[F[_]] {
  this: ModuleSuite[F] with Module[F] with EnumeratorModule[F] with IterateeModule[F] =>

  case class EnumeratorAndValues[A](enumerator: Enumerator[F, A], values: Vector[A]) {
    def resultWithLeftovers[Z](iteratee: Iteratee[F, A, Z]): F[(Z, Vector[A])] = enumerator.into(
      iteratee.flatMap(result => consume[A].map(leftovers => (result, leftovers))(F))(F)
    )(F)
  }

  private[this] val maxDepth = 4

  private[this] def appendGenerator[A: Arbitrary](depth: Int): List[Gen[EnumeratorAndValues[A]]] =
    if (depth < maxDepth) List(
      for {
        EnumeratorAndValues(enumerator1, list1) <- generate[A](depth + 1)
        EnumeratorAndValues(enumerator2, list2) <- generate[A](depth + 1)
      } yield EnumeratorAndValues(enumerator1.append(enumerator2)(F), list1 ++ list2)
    ) else Nil

  private[this] def generate[A](depth: Int)(implicit A: Arbitrary[A]): Gen[EnumeratorAndValues[A]] =
    Gen.oneOf(
      Gen.const(EnumeratorAndValues[A](empty, Vector.empty)),
      A.arbitrary.map(a => EnumeratorAndValues(enumOne(a), Vector(a))),
      (
        for {
          vals <- Gen.listOf(A.arbitrary)
          enumerator <- Gen.oneOf(
            enumStream(vals.toStream),
            enumList(vals),
            enumVector(vals.toVector)
          )
        } yield EnumeratorAndValues(enumerator, vals.toVector)
      ) :: appendGenerator[A](depth): _*
    )

  implicit def arbitraryEnumeratorAndValues[A: Arbitrary]: Arbitrary[EnumeratorAndValues[A]] =
    Arbitrary(generate(0))
}
