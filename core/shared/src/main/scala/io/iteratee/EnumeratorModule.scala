package io.iteratee

import cats.{ Applicative, Monad }

trait EnumeratorModule[F[_]] {
  /**
   * Lift an effectful value into an enumerator.
   */
  final def liftToEnumerator[E](fe: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.liftM(fe)

  final def empty[E](implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.empty

  /** 
   * An enumerator that is at EOF.
   */
  final def enumEnd[E](implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.enumEnd

  class PerformPartiallyApplied[E] {
    def apply[A](fa: F[A])(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.perform(fa)
  }

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  final def perform[E]: PerformPartiallyApplied[E] = new PerformPartiallyApplied[E]

  /**
   * An enumerator that produces a single value.
   */
  final def enumOne[E](e: E)(implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.enumOne(e)

  /**
   * An enumerator that produces values from a stream.
   */
  final def enumStream[E](es: Stream[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumStream(es)

  /**
   * An enumerator that produces values from a list.
   */
  final def enumList[E](es: List[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumList(es)

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[E](es: Vector[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumVector(es)

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  final def enumIndexedSeq[E](es: IndexedSeq[E], min: Int = 0, max: Int = Int.MaxValue)(implicit
    F: Monad[F]
  ): Enumerator[F, E] = Enumerator.enumIndexedSeq(es, min, max)

  /**
   * An enumerator that repeats a given value indefinitely.
   */
  final def repeat[E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.repeat(e)

  /**
   * An enumerator that iteratively performs an operation.
   */
  final def iterate[E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterate(init)(f)
}
