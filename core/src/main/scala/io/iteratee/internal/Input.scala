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
   * Reduce this [[Input]] to a value using the given function.
   */
  final def fold[Z](f: NonEmptyVector[E] => Z): Z = foldWith(
    new Input.Folder[E, Z] {
      final def onEl(e: E): Z = f(NonEmptyVector(e))
      final def onChunk(h: E, t: NonEmptyVector[E]): Z = f(NonEmptyVector(h, t.head +: t.tail))
    }
  )

  /**
   * Reduce this [[Input]] to a value using the given folder.
   *
   * This method is provided primarily for internal use and for cases where the
   * expense of allocating multiple function objects and collection instances is
   * known to be too high. In most cases [[fold]] should be preferred.
   */
  def foldWith[Z](folder: Input.Folder[E, Z]): Z

  def map[B](f: E => B): Input[B]
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
    def onChunk(h: E, t: NonEmptyVector[E]): Z
  }

  private[iteratee] final def fromVectorUnsafe[E](es: Vector[E]): Input[E] =
    if (es.size == 1) el(es.head) else chunk(es.head, NonEmptyVector(es(1), es.drop(2)))

  final def fromNonEmpty[E](es: NonEmptyVector[E]): Input[E] =
    if (es.tail.isEmpty) el(es.head) else chunk(es.head, NonEmptyVector(es.tail.head, es.tail.tail))

  /**
   * An input value containing a single element.
   */
  final def el[E](e: E): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onEl(e)
    final def map[B](f: E => B): Input[B] = el(f(e))
  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](h: E, t: NonEmptyVector[E]): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onChunk(h, t)
    final def map[B](f: E => B): Input[B] = chunk(f(h), NonEmptyVector(f(t.head), t.tail.map(f)))
  }
}
