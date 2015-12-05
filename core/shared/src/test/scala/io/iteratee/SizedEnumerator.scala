package io.iteratee

import algebra.Monoid
import cats.Eval
import org.scalacheck.{ Arbitrary, Gen }

trait SizedEnumerator[A] {
	def source: Stream[A]
	def enumerator: Enumerator[Eval, A]
}

trait SizedEnumeratorCompanion[E[x] <: SizedEnumerator[x]] {
  def maxGroupSize: Int
  def maxGroupCount: Int

  def apply[A](source: Stream[A], enumerator: Enumerator[Eval, A]): E[A]

  def pair[A](implicit A: Arbitrary[A]): Gen[(Stream[A], Enumerator[Eval, A])] =
    Gen.oneOf(
      Gen.const((Stream.empty[A], Enumerator.empty[Eval, A])),
      A.arbitrary.map(a => (Stream(a), Enumerator.enumOne[Eval, A](a))),
      for {
        size <- Gen.chooseNum(0, maxGroupSize)
        vals <- Gen.containerOfN[Stream, A](size, A.arbitrary)
        enum <- Gen.oneOf(
          Enumerator.enumStream[Eval, A](vals),
          Enumerator.enumList[Eval, A](vals.toList)
        )
      } yield (vals, enum)
    )

  implicit def arbitrarySizedEnumerator[A](implicit
  	A: Arbitrary[A]
  ): Arbitrary[E[A]] =
    Arbitrary(
    	for {
        size <- Gen.chooseNum(0, maxGroupCount)
        groups <- Gen.containerOfN[Stream, (Stream[A], Enumerator[Eval, A])](size, pair[A])
        (ss, es) = groups.unzip
      } yield apply(ss.flatten, Monoid[Enumerator[Eval, A]].combineAll(es))
    )
}

case class SmallEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[Eval, A]
) extends SizedEnumerator[A]

object SmallEnumerator extends SizedEnumeratorCompanion[SmallEnumerator] {
	val maxGroupSize: Int = 8
  val maxGroupCount: Int = 4
}

case class LargeEnumerator[A](
	source: Stream[A],
	enumerator: Enumerator[Eval, A]
) extends SizedEnumerator[A]

object LargeEnumerator extends SizedEnumeratorCompanion[LargeEnumerator] {
	val maxGroupSize: Int = 1024
  val maxGroupCount: Int = 8
}
