package io.iteratee

import cats.Eval
import org.scalacheck.{ Arbitrary, Gen }

trait SizedEnumerator[A] {
	def source: Stream[A]
	def enumerator: Enumerator[A, Eval]
}

trait SizedEnumeratorCompanion[E[x] <: SizedEnumerator[x]] {
  def maxSize: Int
  def apply[A](source: Stream[A], enumerator: Enumerator[A, Eval]): E[A]

  implicit def arbitrarySizedEnumerator[A](implicit
  	A: Arbitrary[A]
  ): Arbitrary[E[A]] =
    Arbitrary(
    	for {
        size <- Gen.chooseNum(0, maxSize)
        vals <- Gen.containerOfN[Stream, A](size, A.arbitrary)
        enum <- Gen.oneOf(
          Enumerator.enumStream[A, Eval](vals),
          Enumerator.enumList[A, Eval](vals.toList)
        )
      } yield apply(vals, enum)
    )
}

case class SmallEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[A, Eval]
) extends SizedEnumerator[A]

object SmallEnumerator extends SizedEnumeratorCompanion[SmallEnumerator] {
	val maxSize: Int = 8
}

case class LargeEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[A, Eval]
) extends SizedEnumerator[A]

object LargeEnumerator extends SizedEnumeratorCompanion[LargeEnumerator] {
	val maxSize: Int = 1024
}
