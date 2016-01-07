package io.iteratee

import algebra.Eq
import cats.Monad

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
  final def map[O, I](f: O => I)(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.map(f)

  /**
   * Map a function returning a value in a context over a stream.
   *
   * @group Enumeratees
   */
  final def mapK[O, I](f: O => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.mapK(f)

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   *
   * @group Enumeratees
   */
  final def flatMap[O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.flatMap(f)

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   *
   * @group Enumeratees
   */
  final def collect[O, I](pf: PartialFunction[O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] = Enumeratee.collect(pf)

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, E] = Enumeratee.filter(p)

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
  final def uniq[E: Eq](implicit F: Monad[F]): Enumeratee[F, E, E] = Enumeratee.uniq

  /**
   * Zip with the number of elements that have been encountered.
   *
   * @group Enumeratees
   */
  final def zipWithIndex[E](implicit F: Monad[F]): Enumeratee[F, E, (E, Long)] = Enumeratee.zipWithIndex

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
