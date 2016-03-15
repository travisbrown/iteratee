package io.iteratee

import cats.{ Applicative, Monad, MonadError }

/**
 * @groupname Enumerators Enumerators
 * @groupprio Enumerators 0
 *
 * @groupname Helpers Helper classes
 * @groupprio Helpers 4
 */
trait EnumeratorModule[F[_]] {
  /**
   * Lift an effectful value into an enumerator.
   *
   * @group Enumerators
   */
  final def liftToEnumerator[E](fe: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.liftM(fe)

  /**
   * @group Helpers
   */
  sealed class FailEnumeratorPartiallyApplied[E] {
    final def apply[T](e: T)(implicit F: MonadError[F, T]): Enumerator[F, E] = Enumerator.fail(e)
  }

  /**
   * Create a failed enumerator with the given error.
   *
   * @group Enumerators
   */
  final def failEnumerator[E]: FailEnumeratorPartiallyApplied[E] = new FailEnumeratorPartiallyApplied[E]

  /**
   * An empty enumerator.
   *
   * @group Enumerators
   */
  final def empty[E](implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.empty

  /**
   * @group Helpers
   */
  sealed class PerformPartiallyApplied[E] {
    final def apply[A](fa: F[A])(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.perform(fa)
  }

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   *
   * @group Enumerators
   */
  final def perform[E]: PerformPartiallyApplied[E] = new PerformPartiallyApplied[E]

  /**
   * An enumerator that produces a single value.
   *
   * @group Enumerators
   */
  final def enumOne[E](e: E)(implicit F: Applicative[F]): Enumerator[F, E] = Enumerator.enumOne(e)

  /**
   * An enumerator that produces values from a stream.
   *
   * @group Enumerators
   */
  final def enumStream[E](es: Stream[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumStream(es)

  /**
   * An enumerator that produces values from a list.
   *
   * @group Enumerators
   */
  final def enumList[E](es: List[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumList(es)

  /**
   * An enumerator that produces values from a vector.
   *
   * @group Enumerators
   */
  final def enumVector[E](es: Vector[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.enumVector(es)

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   *
   * @group Enumerators
   */
  final def enumIndexedSeq[E](es: IndexedSeq[E], min: Int = 0, max: Int = Int.MaxValue)
    (implicit F: Monad[F]): Enumerator[F, E] =
      Enumerator.enumIndexedSeq(es, min, max)

  /**
   * An enumerator that repeats the given value indefinitely.
   *
   * @group Enumerators
   */
  final def repeat[E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.repeat(e)

  /**
   * An enumerator that iteratively performs an operation and returns the
   * results.
   *
   * @group Enumerators
   */
  final def iterate[E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterate(init)(f)

  /**
   * An enumerator that iteratively performs an effectful operation and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateM[E](init: E)(f: E => F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterateM(init)(f)

  /**
   * An enumerator that iteratively performs an operation until None is produced and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateUntil[E](init: E)(f: E => Option[E])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterateUntil(init)(f)

  /**
   * An enumerator that iteratively performs an effectful operation until None is produced and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateUntilM[E](init: E)(f: E => F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
    Enumerator.iterateUntilM(init)(f)

  /**
   * An enumerator that returns the result of an effectful operation until
   * `None` is generated.
   */
  final def generateM[E](f: F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] = Enumerator.generateM(f)
}
