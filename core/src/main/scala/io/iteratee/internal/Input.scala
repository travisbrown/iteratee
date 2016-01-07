package io.iteratee.internal

/**
 * An input to an [[io.iteratee.Iteratee]].
 *
 * This type is isomorphic to a [[cats.data.NonEmptyVector]], but exists as a
 * separate type for the sake of performance (using `NonEmptyVector` proved
 * significantly slower in experiments).
 *
 * @tparam E The element type
 */
sealed abstract class Input[E] extends Serializable {
  /**
   * Reduce this [[Input]] to a value using the given folder.
   */
  def foldWith[Z](folder: Input.Folder[E, Z]): Z

  /**
   * The number of elements in this [[Input]].
   */
  def size: Int

  /**
   * Combine two inputs.
   */
  def append(other: Input[E]): Input[E]

  /**
   * Convert this [[Input]] to a (non-empty)
   * [[scala.collection.immutable.Vector]].
   */
  def toVector: Vector[E]
}

final object Input  {
  /**
   * Represents two functions that can be used to reduce an [[Input]] to a
   * value.
   *
   * Combining these "functions" into a single trait allows us to let objects
   * we're already creating (e.g. a [[Step]]) play this role for us.
   *
   * @tparam E The element type
   * @tparam Z The result type
   */
  trait Folder[E, Z] {
    def onEl(e: E): Z
    def onChunk(h1: E, h2: E, t: Vector[E]): Z
  }

  private[internal] final def fromVectorUnsafe[E](es: Vector[E]): Input[E] =
    if (es.size == 1) el(es(0)) else chunk(es(0), es(1), es.drop(2))

  private[internal] final def fromPair[E](e: E, es: Vector[E]): Input[E] =
    if (es.isEmpty) el(e) else chunk(e, es.head, es.tail)

  /**
   * An input value containing a single element.
   */
  final def el[E](e: E): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onEl(e)
    final def size: Int = 1
    final def append(other: Input[E]): Input[E] = other.foldWith(
      new Folder[E, Input[E]] {
        def onEl(otherE: E): Input[E] = chunk(e, otherE, Vector.empty)
        def onChunk(h1: E, h2: E, t: Vector[E]): Input[E] = chunk(e, h1, h2 +: t)
      }
    )
    final def toVector: Vector[E] = Vector(e)
  }

  /**
   * An input value containing two or more elements.
   */
  final def chunk[E](h1: E, h2: E, t: Vector[E]): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onChunk(h1, h2, t)
    final def size: Int = 2 + t.size
    final def append(other: Input[E]): Input[E] = chunk(h1, h2, t ++ other.toVector)
    final def toVector: Vector[E] = h1 +: h2 +: t
  }
}
