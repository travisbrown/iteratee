package io

import cats.{ Applicative, Eval, Foldable, Semigroup, Traverse }

package iteratee {
  final case class NonEmptyVector[A](head: A, tail: Vector[A]) {
    def toVector: Vector[A] = head +: tail
    def length: Int = tail.size + 1
    def reduce(implicit S: Semigroup[A]): A = S.combineAllOption(toVector).get
    def map[B](f: A => B): NonEmptyVector[B] = NonEmptyVector(f(head), tail.map(f))
    def foldLeft[B](b: B)(f: (B, A) => B): B = tail.foldLeft(f(b, head))(f)
    def toCatsNonEmptyVector: cats.data.NonEmptyVector[A] = cats.data.NonEmptyVector(head, tail)
  }

  final object NonEmptyVector {
    private[this] val F: Traverse[Vector] = cats.instances.vector.catsStdInstancesForVector

    def fromVectorUnsafe[A](vector: Vector[A]): NonEmptyVector[A] = NonEmptyVector(vector.head, vector.tail)

    implicit val catsDataInstancesForNonEmptyVector: Traverse[NonEmptyVector] = new Traverse[NonEmptyVector] {
      def foldLeft[A, B](fa: NonEmptyVector[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
      def foldRight[A, B](fa: NonEmptyVector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.defer(f(fa.head, F.foldRight(fa.tail, lb)(f)))
      def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyVector[B]] =
        G.map2Eval(f(fa.head), Eval.always(F.traverse(fa.tail)(f)))(NonEmptyVector(_, _)).value
    }
  }
}
