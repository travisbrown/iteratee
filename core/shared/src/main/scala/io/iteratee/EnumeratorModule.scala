package io.iteratee

import cats.{ Applicative, Monad }

trait EnumeratorModule[F[_]] {
  /**
   * Lift an effectful value into an enumerator.
   */
  final def liftToEnumerator[E](fe: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.liftM[F, E](fe)

  final def empty[E](implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.empty[F, E]

  /** 
   * An enumerator that is at EOF.
   */
  final def enumEnd[E](implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.enumEnd[F, E]

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  final def perform[E, A](f: F[A])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.perform[F, E, A](f)

  /**
   * An enumerator that produces a single value.
   */
  final def enumOne[E](e: E)(implicit F: Applicative[F]): Enumerator[F, E] =
    Enumerator.enumOne[F, E](e)

  /**
   * An enumerator that produces values from a stream.
   */
  final def enumStream[E](es: Stream[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumStream[F, E](es)

  /**
   * An enumerator that produces values from a list.
   */
  final def enumList[E](es: List[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumList[F, E](es)

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[E](es: Vector[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumVector[F, E](es)

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  final def enumIndexedSeq[E](es: IndexedSeq[E], min: Int = 0, max: Int = Int.MaxValue)(implicit
    F: Monad[F]
  ): Enumerator[F, E] = Enumerator.enumIndexedSeq[F, E](es, min, max)

  /**
   * An enumerator that repeats a given value indefinitely.
   */
  final def repeat[E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.repeat[F, E](e)

  /**
   * An enumerator that iteratively performs an operation.
   */
  final def iterate[E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterate[F, E](init)(f)
}
