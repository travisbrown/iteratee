package io.iteratee.internal

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
   * Reduce this [[Input]] to a value using the given pair of functions.
   */
  final def fold[Z](end: => Z, values: Vector[E] => Z): Z = foldWith(
    new Input.Folder[E, Z] {
      final def onEnd: Z = end
      final def onEl(e: E): Z = values(Vector(e))
      final def onChunk(h1: E, h2: E, es: Vector[E]): Z = values(h1 +: h2 +: es)
    }
  )

  /**
   * Reduce this [[Input]] to a value using the given four functions.
   *
   * This method is provided primarily for internal use and for cases where the
   * expense of allocating multiple function objects and collection instances is
   * known to be too high. In most cases [[fold]] should be preferred.
   */
  def foldWith[Z](folder: Input.Folder[E, Z]): Z

  def isEnd: Boolean

  /**
   * Convert this [[Input]] value into a sequence of elements.
   */
  private[iteratee] def toVector: Vector[E]
}

final object Input extends InputInstances {
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
  abstract class Folder[@specialized E, Z] extends Serializable {
    def onEnd: Z
    def onEl(e: E): Z
    def onChunk(h1: E, h2: E, es: Vector[E]): Z
  }

  /**
   * An input value representing the end of a stream.
   */
  final def end[E]: Input[E] = endValue.asInstanceOf[Input[E]]

  /**
   * An input value containing a single element.
   */
  final def el[E](e: E): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onEl(e)
    final def isEnd: Boolean = false
    private[iteratee] final def toVector: Vector[E] = Vector(e)
  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](e1: E, e2: E, es: Vector[E]): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onChunk(e1, e2, es)
    final def isEnd: Boolean = false
    private[iteratee] final def toVector: Vector[E] = e1 +: e2 +: es
  }

  /**
   * We define a single end-of-stream value and cast it to the appropriate type
   * in `end` in order to avoid allocations.
   */
  private[this] final val endValue: Input[Nothing] = new Input[Nothing] {
    final def foldWith[A](folder: Folder[Nothing, A]): A = folder.onEnd
    final val isEnd: Boolean = true
    private[iteratee] final val toVector: Vector[Nothing] = Vector.empty
  }
}
