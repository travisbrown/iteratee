package io.iteratee

import algebra.Eq
import cats.{ Applicative, Monad }

/**
 * @groupname Enumeratees Enumeratees
 * @groupprio Enumeratees 2
 */
trait EnumerateeModule[F[_]] {
  /**
   * Map a function over a stream.
   *
   * @group Enumeratees
   */
  final def map[O, I](f: O => I)(implicit F: Applicative[F]): Enumeratee[F, O, I] = Enumeratee.map(f)

  /**
   * Map a function returning a value in a context over a stream.
   *
   * @group Enumeratees
   */
  @deprecated("Use flatMapF", "0.3.0")
  final def mapK[O, I](f: O => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.flatMapF(f)

  /**
   * Map a function returning a value in a context over a stream.
   *
   * @group Enumeratees
   */
  final def flatMapF[O, I](f: O => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.flatMapF(f)

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   *
   * @group Enumeratees
   */
  final def flatMap[O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.flatMap(f)

  /**
   * An [[Enumeratee]] that takes a given number of the first values in a
   * stream.
   */
  final def take[E](n: Int)(implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.take(n)

  /**
   * An [[Enumeratee]] that tales values from a stream as long as they satisfy
   * the given predicate.
   */
  final def takeWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.takeWhile(p)

  /**
   * An [[Enumeratee]] that drops a given number of the first values in a
   * stream.
   */
  final def drop[E](n: Int)(implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.drop(n)

  /**
   * An [[Enumeratee]] that drops values from a stream as long as they satisfy
   * the given predicate.
   */
  final def dropWhile[E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.dropWhile(p)

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   *
   * @group Enumeratees
   */
  final def collect[O, I](pf: PartialFunction[O, I])(implicit F: Applicative[F]): Enumeratee[F, O, I] =
    Enumeratee.collect(pf)

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.filter(p)

  /**
    * Drop values that do not satisfy the given monadic predicate.
    *
    * @group Enumeratees
    */
  @deprecated("Use filterF", "0.3.0")
  final def filterK[E](p: E => F[Boolean])(implicit F: Monad[F]): Enumeratee[F, E, E] = Enumeratee.filterF(p)

  /**
    * Drop values that do not satisfy the given monadic predicate.
    *
    * @group Enumeratees
    */
  final def filterF[E](p: E => F[Boolean])(implicit F: Monad[F]): Enumeratee[F, E, E] = Enumeratee.filterF(p)

  /**
   * Apply the given [[Iteratee]] repeatedly.
   *
   * @group Enumeratees
   */
  final def sequenceI[O, I](iteratee: Iteratee[F, O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    Enumeratee.sequenceI(iteratee)

  /**
   * Collapse consecutive duplicates.
   *
   * @note Assumes that the stream is sorted.
   * @group Enumeratees
   */
  final def uniq[E: Eq](implicit F: Applicative[F]): Enumeratee[F, E, E] = Enumeratee.uniq[F, E]

  /**
   * Zip with the number of elements that have been encountered.
   *
   * @group Enumeratees
   */
  final def zipWithIndex[E](implicit F: Applicative[F]): Enumeratee[F, E, (E, Long)] = Enumeratee.zipWithIndex

  /**
   * Split the stream into groups of a given length.
   *
   * @group Enumeratees
   */
  final def grouped[E](n: Int)(implicit F: Monad[F]): Enumeratee[F, E, Vector[E]] = Enumeratee.grouped(n)

  /**
   * Split the stream using the given predicate to identify delimiters.
   *
   * @group Enumeratees
   */
  final def splitOn[E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, Vector[E]] = Enumeratee.splitOn(p)

  /**
   * Transform a stream by taking the cross-product with the given
   * [[Enumerator]].
   *
   * @group Enumeratees
   */
  final def cross[E1, E2](e2: Enumerator[F, E2])(implicit F: Monad[F]): Enumeratee[F, E1, (E1, E2)] =
   Enumeratee.cross(e2)
}
