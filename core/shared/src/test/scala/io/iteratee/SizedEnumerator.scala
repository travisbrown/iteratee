package io.iteratee

import algebra.Monoid
import cats.Eval
import org.scalacheck.{ Arbitrary, Gen }

trait SizedEnumerator[A] {
	def source: Stream[A]
	def enumerator: Enumerator[A, Eval]
}

trait SizedEnumeratorCompanion[E[x] <: SizedEnumerator[x]] {
  def maxGroupSize: Int
  def maxGroupCount: Int

  def apply[A](source: Stream[A], enumerator: Enumerator[A, Eval]): E[A]

  def pair[A](implicit A: Arbitrary[A]): Gen[(Stream[A], Enumerator[A, Eval])] =
    Gen.oneOf(
      Gen.const((Stream.empty[A], Enumerator.empty[A, Eval])),
      A.arbitrary.map(a => (Stream(a), Enumerator.enumOne[A, Eval](a))),
      for {
        size <- Gen.chooseNum(0, maxGroupSize)
        vals <- Gen.containerOfN[Stream, A](size, A.arbitrary)
        enum <- Gen.oneOf(
          Enumerator.enumStream[A, Eval](vals),
          Enumerator.enumList[A, Eval](vals.toList)
        )
      } yield (vals, enum)
    )

  implicit def arbitrarySizedEnumerator[A](implicit
  	A: Arbitrary[A]
  ): Arbitrary[E[A]] =
    Arbitrary(
    	for {
        size <- Gen.chooseNum(0, maxGroupCount)
        groups <- Gen.containerOfN[Stream, (Stream[A], Enumerator[A, Eval])](size, pair[A])
        (ss, es) = groups.unzip
      } yield apply(ss.flatten, Monoid[Enumerator[A, Eval]].combineAll(es))
    )
}

case class SmallEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[A, Eval]
) extends SizedEnumerator[A]

object SmallEnumerator extends SizedEnumeratorCompanion[SmallEnumerator] {
	val maxGroupSize: Int = 8
  val maxGroupCount: Int = 4
}

case class LargeEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[A, Eval]
) extends SizedEnumerator[A]

object LargeEnumerator extends SizedEnumeratorCompanion[LargeEnumerator] {
	val maxGroupSize: Int = 1024
  val maxGroupCount: Int = 8
}
