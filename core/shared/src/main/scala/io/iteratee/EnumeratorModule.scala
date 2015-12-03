package io.iteratee

import cats.{ Applicative, Monad }

trait EnumeratorModule[F[_]] {
  def liftM[E](fe: F[E])(implicit F: Monad[F]): Enumerator[E, F] = Enumerator.liftM[F, E](fe)
  def empty[E](implicit F: Applicative[F]): Enumerator[E, F] = Enumerator.empty[E, F]

  /** 
   * An Enumerator that is at EOF
   */
  def enumEnd[E](implicit F: Applicative[F]): Enumerator[E, F] = Enumerator.enumEnd[E, F]

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  def perform[E, A](f: F[A])(implicit F: Monad[F]): Enumerator[E, F] =
    Enumerator.perform[E, F, A](f)

  /**
   * An enumerator that produces a single value.
   */
  def enumOne[E](e: E)(implicit F: Applicative[F]): Enumerator[E, F] = Enumerator.enumOne[E, F](e)

  /**
   * An enumerator that produces values from a stream.
   */
  def enumStream[E](es: Stream[E])(implicit F: Monad[F]): Enumerator[E, F] =
    Enumerator.enumStream[E, F](es)

  /**
   * An enumerator that produces values from a list.
   */
  def enumList[E](es: List[E])(implicit F: Monad[F]): Enumerator[E, F] =
    Enumerator.enumList[E, F](es)

  /**
   * An enumerator that produces values from a vector.
   */
  def enumVector[E](es: Vector[E])(implicit F: Monad[F]): Enumerator[E, F] =
    Enumerator.enumVector[E, F](es)

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  def enumIndexedSeq[E](es: IndexedSeq[E], min: Int = 0, max: Int = Int.MaxValue)(implicit
    F: Monad[F]
  ): Enumerator[E, F] = Enumerator.enumIndexedSeq[E, F](es, min, max)

  /**
   * An enumerator that repeats a given value indefinitely.
   */
  def repeat[E](e: E)(implicit F: Monad[F]): Enumerator[E, F] = Enumerator.repeat[E, F](e)

  /**
   * An enumerator that iteratively performs an operation.
   */
  def iterate[E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[E, F] =
    Enumerator.iterate[E, F](init)(f)
}
