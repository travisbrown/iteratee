package io.iteratee

import algebra.Eq

/**
 * @groupname Enumeratees Enumeratees
 * @groupprio Enumeratees 2
 */
trait EnumerateeModule[F[_]] { this: Module[F] =>
  /**
   * Map a function over a stream.
   *
   * @group Enumeratees
   */
  final def map[O, I](f: O => I): Enumeratee[F, O, I] = Enumeratee.map(f)(F)

  /**
   * Map a function returning a value in a context over a stream.
   *
   * @group Enumeratees
   */
  @deprecated("Use flatMapM", "0.4.1")
  final def flatMapF[O, I](f: O => F[I]): Enumeratee[F, O, I] = flatMapM(f)

  /**
   * Map a function returning a value in a context over a stream.
   *
   * @group Enumeratees
   */
  final def flatMapM[O, I](f: O => F[I]): Enumeratee[F, O, I] = Enumeratee.flatMapM(f)(F)

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   *
   * @group Enumeratees
   */
  final def flatMap[O, I](f: O => Enumerator[F, I]): Enumeratee[F, O, I] = Enumeratee.flatMap(f)(F)

  /**
   * An [[Enumeratee]] that takes a given number of the first values in a
   * stream.
   *
   * @group Enumeratees
   */
  final def take[E](n: Long): Enumeratee[F, E, E] = Enumeratee.take(n)(F)

  /**
   * An [[Enumeratee]] that tales values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Enumeratees
   */
  final def takeWhile[E](p: E => Boolean): Enumeratee[F, E, E] = Enumeratee.takeWhile(p)(F)

  /**
   * An [[Enumeratee]] that drops a given number of the first values in a
   * stream.
   *
   * @group Enumeratees
   */
  final def drop[E](n: Long): Enumeratee[F, E, E] = Enumeratee.drop(n)(F)

  /**
   * An [[Enumeratee]] that drops values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Enumeratees
   */
  final def dropWhile[E](p: E => Boolean): Enumeratee[F, E, E] = Enumeratee.dropWhile(p)(F)

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   *
   * @group Enumeratees
   */
  final def collect[O, I](pf: PartialFunction[O, I]): Enumeratee[F, O, I] = Enumeratee.collect(pf)(F)

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[E](p: E => Boolean): Enumeratee[F, E, E] = Enumeratee.filter(p)(F)

  /**
    * Drop values that do not satisfy the given monadic predicate.
    *
    * @group Enumeratees
    */
  @deprecated("Use filterM", "0.4.1")
  final def filterF[E](p: E => F[Boolean]): Enumeratee[F, E, E] = filterM(p)

  /**
    * Drop values that do not satisfy the given monadic predicate.
    *
    * @group Enumeratees
    */
  final def filterM[E](p: E => F[Boolean]): Enumeratee[F, E, E] = Enumeratee.filterM(p)(F)

  /**
   * Apply the given [[Iteratee]] repeatedly.
   *
   * @group Enumeratees
   */
  final def sequenceI[O, I](iteratee: Iteratee[F, O, I]): Enumeratee[F, O, I] = Enumeratee.sequenceI(iteratee)(F)

  /**
   * Collapse consecutive duplicates.
   *
   * @note Assumes that the stream is sorted.
   * @group Enumeratees
   */
  final def uniq[E](implicit E: Eq[E]): Enumeratee[F, E, E] = Enumeratee.uniq[F, E](F, E)

  /**
   * Zip with the number of elements that have been encountered.
   *
   * @group Enumeratees
   */
  final def zipWithIndex[E]: Enumeratee[F, E, (E, Long)] = Enumeratee.zipWithIndex(F)

  /**
   * Split the stream into groups of a given length.
   *
   * @group Enumeratees
   */
  final def grouped[E](n: Int): Enumeratee[F, E, Vector[E]] = Enumeratee.grouped(n)(F)

  /**
   * Split the stream using the given predicate to identify delimiters.
   *
   * @group Enumeratees
   */
  final def splitOn[E](p: E => Boolean): Enumeratee[F, E, Vector[E]] = Enumeratee.splitOn(p)(F)

  /**
   * Transform a stream by taking the cross-product with the given
   * [[Enumerator]].
   *
   * @group Enumeratees
   */
  final def cross[E1, E2](e2: Enumerator[F, E2]): Enumeratee[F, E1, (E1, E2)] = Enumeratee.cross(e2)(F)

  /**
   * Add a value `delim` between every two items in a stream.
   *
   * @group Enumeratees
   */
  final def intersperse[E](delim: E): Enumeratee[F, E, E] = Enumeratee.intersperse(delim)(F)
}
