package io.iteratee

import cats.MonadError

/**
 * @groupname Enumerators Enumerators
 * @groupprio Enumerators 0
 *
 * @groupname Helpers Helper classes
 * @groupprio Helpers 4
 */
trait EnumeratorModule[F[_]] { this: Module[F] =>
  private[this] final def defaultChunkSize: Int = 1024

  /**
   * Lift an effectful value into an enumerator.
   *
   * @group Enumerators
   */
  final def liftToEnumerator[E](fe: F[E]): Enumerator[F, E] = Enumerator.liftM(fe)(F)

  /**
   * An enumerator that produces the given values.
   *
   * @group Enumerators
   */
  final def enumerate[E](xs: E*): Enumerator[F, E] = Enumerator.enumerate(xs: _*)(F)

  /**
   * An empty enumerator.
   *
   * @group Enumerators
   */
  final def empty[E]: Enumerator[F, E] = Enumerator.empty(F)

  /**
   * @group Helpers
   */
  sealed class PerformPartiallyApplied[E] {
    final def apply[A](fa: F[A]): Enumerator[F, E] = Enumerator.perform(fa)(F)
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
  final def enumOne[E](e: E): Enumerator[F, E] = Enumerator.enumOne(e)(F)

  /**
   * An enumerator that produces values from an iterable collection.
   *
   * @group Enumerators
   */
  final def enumIterable[E](es: Iterable[E], chunkSize: Int = defaultChunkSize): Enumerator[F, E] =
    Enumerator.enumIterable(es, chunkSize)(F)

  /**
   * An enumerator that produces values from a stream.
   *
   * @group Enumerators
   */
  final def enumStream[E](es: Stream[E], chunkSize: Int = defaultChunkSize): Enumerator[F, E] =
    Enumerator.enumStream(es, chunkSize)(F)

  /**
   * An enumerator that produces values from a list.
   *
   * @group Enumerators
   */
  final def enumList[E](es: List[E]): Enumerator[F, E] = Enumerator.enumList(es)(F)

  /**
   * An enumerator that produces values from a vector.
   *
   * @group Enumerators
   */
  final def enumVector[E](es: Vector[E]): Enumerator[F, E] = Enumerator.enumVector(es)(F)

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   *
   * @group Enumerators
   */
  final def enumIndexedSeq[E](es: IndexedSeq[E], min: Int = 0, max: Int = Int.MaxValue): Enumerator[F, E] =
    Enumerator.enumIndexedSeq(es, min, max)(F)

  /**
   * An enumerator that repeats the given value indefinitely.
   *
   * @group Enumerators
   */
  final def repeat[E](e: E): Enumerator[F, E] = Enumerator.repeat(e)(F)

  /**
   * An enumerator that iteratively performs an operation and returns the
   * results.
   *
   * @group Enumerators
   */
  final def iterate[E](init: E)(f: E => E): Enumerator[F, E] = Enumerator.iterate(init)(f)(F)

  /**
   * An enumerator that iteratively performs an effectful operation and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateM[E](init: E)(f: E => F[E]): Enumerator[F, E] = Enumerator.iterateM(init)(f)(F)

  /**
   * An enumerator that iteratively performs an operation until None is produced and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateUntil[E](init: E)(f: E => Option[E]): Enumerator[F, E] = Enumerator.iterateUntil(init)(f)(F)

  /**
   * An enumerator that iteratively performs an effectful operation until None is produced and returns
   * the results.
   *
   * @group Enumerators
   */
  final def iterateUntilM[E](init: E)(f: E => F[Option[E]]): Enumerator[F, E] = Enumerator.iterateUntilM(init)(f)(F)

  /**
   * An enumerator that returns the result of an effectful operation until
   * `None` is generated.
   *
   * @group Enumerators
   */
  final def generateM[E](f: F[Option[E]]): Enumerator[F, E] = Enumerator.generateM(f)(F)
}

trait EnumeratorErrorModule[F[_], T] extends EnumeratorModule[F] {
  this: Module[F] { type M[f[_]] <: MonadError[f, T] } =>

  /**
   * Create a failed enumerator with the given error.
   *
   * @group Iteratees
   */
  final def failEnumerator[E](t: T): Enumerator[F, E] = Enumerator.fail(t)(F)

  /**
   * An enumerator that either produces a single value or fails.
   *
   * @group Iteratees
   */
  final def enumEither[E](either: Either[T, E]): Enumerator[F, E] = Enumerator.enumEither(either)(F)
}
