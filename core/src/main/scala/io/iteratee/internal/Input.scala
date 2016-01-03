package io.iteratee.internal

import cats.data.NonEmptyVector

/**
 * An input to an [[Iteratee]].
 *
 * An input value can signal the end of a stream ([[Input.end]]) or it can
 * contain one or more values of the element type. Non-end-of-stream inputs
 * could in principle be represented by a collection of elements, but for the
 * sake of performance we provide special constructors for inputs that contain
 * a single element ([[Input.el]]).
 *
 * @tparam E The element type
 */
sealed abstract class Input[@specialized E] extends Serializable {
  /**
   * Reduce this [[Input]] to a value using the given folder.
   *
   * This method is provided primarily for internal use and for cases where the
   * expense of allocating multiple function objects and collection instances is
   * known to be too high. In most cases [[fold]] should be preferred.
   */
  def foldWith[Z](folder: Input.Folder[E, Z]): Z

  def map[B](f: E => B): Input[B]

  def toNonEmpty: NonEmptyVector[E]
}

final object Input  {
  /**
   * Represents four functions that can be used to reduce an [[Input]] to a
   * value.
   *
   * Combining three "functions" into a single class allows us to save
   * allocations. `onEl` must be consistent with `onChunk`.
   *
   * @tparam E The element type
   * @tparam Z The result type
   */
  trait Folder[@specialized E, Z] {
    def onEl(e: E): Z
    def onChunk(h1: E, h2: E, t: Vector[E]): Z
  }

  private[iteratee] final def fromVectorUnsafe[E](es: Vector[E]): Input[E] =
    if (es.size == 1) el(es(0)) else chunk(es(0), es(1), es.drop(2))

  final def fromPair[E](e: E, es: Vector[E]): Input[E] =
    if (es.isEmpty) el(e) else chunk(e, es.head, es.tail)

  /**
   * An input value containing a single element.
   */
  final def el[E](e: E): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onEl(e)
    final def map[B](f: E => B): Input[B] = el(f(e))
    final def toNonEmpty: NonEmptyVector[E] = NonEmptyVector(e)
  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](h1: E, h2: E, t: Vector[E]): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onChunk(h1, h2, t)
    final def map[B](f: E => B): Input[B] = chunk(f(h1), f(h2), t.map(f))
    final def toNonEmpty: NonEmptyVector[E] = NonEmptyVector(h1, h2 +: t)
  }
}
