package io

import cats.{ Semigroup, Traverse }
import cats.data.OneAnd
import cats.instances.VectorInstances

package object iteratee extends VectorInstances {
  final type NonEmptyVector[A] = OneAnd[Vector, A]

  implicit final class NonEmptyVectorOps[A](underlying: OneAnd[Vector, A]) {
    def toVector: Vector[A] = underlying.unwrap
    def length: Int = underlying.length
    def reduce(implicit S: Semigroup[A]): A = S.combineAllOption(toVector).get
  }

  final object NonEmptyVector {
    def apply[A](h: A, t: Vector[A]): NonEmptyVector[A] = OneAnd(h, t)
    def fromVectorUnsafe[A](vector: Vector[A]): NonEmptyVector[A] = OneAnd(vector.head, vector.tail)
    val catsDataInstancesForNonEmptyVector: Traverse[NonEmptyVector] = OneAnd.catsDataTraverseForOneAnd[Vector]
  }
}
