package io.iteratee.internal

/**
 * An input to an [[Iteratee]].
 *
 * An input value can signal the end of a stream ([[Input.end]]) or it can
 * contain one or more values of the element type. Non-end-of-stream inputs
 * could in principle be represented by a collection of elements, but for the
 * sake of performance we provide special constructors for inputs that are empty
 * ([[Input.empty]]) or contain a single element ([[Input.el]]).
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
      final def onChunk(es: Vector[E]): Z = values(es)
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
   * Map a function over all values (if any) in this input.
   */
  def map[B](f: E => B): Input[B]

  /**
   * Map a function that returns an input over all values (if any) in this
   * input and flatten the result.
   */
  def flatMap[B](f: E => Input[B]): Input[B]

  /*
  /**
   * Perform an operation for every value in this input.
   */
  def foreach(f: E => Unit): Unit

  /**
   * Return an input that contains all values in this input that satisfy the
   * given predicate.
   */
  def filter(p: E => Boolean): Input[E]

  /**
   * Check whether all values in this input satisfy the given predicate.
   */
  def forall(p: E => Boolean): Boolean

  /**
   * Check whether any values in this input satisfy the given predicate.
   */
  def exists(p: E => Boolean): Boolean
  */

  /**
   * Normalize the [[Input]] so that representations do not overlap.
   *
   * If this [[Input]] is a chunk with no values, an empty input will be
   * returned, and if it's a chunk with a single value, and element input will
   * be returned.
   */
  private[iteratee] def normalize: Input[E]

  /**
   * Convert this [[Input]] value into a sequence of elements.
   */
  private[iteratee] def toVector: Vector[E]

  /**
   * Return the [[Input]] that contains fewer elements.
   *
   * `Input.end` is defined to be shorter than any other value.
   */
  private[iteratee] final def shorter(that: Input[E]): Input[E] =
    if (isEnd || that.isEnd) Input.end else {
      if (toVector.lengthCompare(that.toVector.size) < 0) this else that
    }
}

final object Input extends InputInstances {
  /**
   * Represents four functions that can be used to reduce an [[Input]] to a
   * value.
   *
   * Combining four "functions" into a single class allows us to save
   * allocations. `onEmpty` and `onEl` must be consistent with `onChunk`.
   *
   * @tparam E The element type
   * @tparam Z The result type
   */
  abstract class Folder[@specialized E, Z] extends Serializable {
    def onEnd: Z
    def onEl(e: E): Z
    def onChunk(es: Vector[E]): Z
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
    final def map[B](f: E => B): Input[B] = Input.el(f(e))
    final def flatMap[B](f: E => Input[B]): Input[B] = f(e)
    /*final def foreach(f: E => Unit): Unit = f(e)
    final def filter(p: E => Boolean): Input[E] = if (p(e)) this else empty
    final def forall(p: E => Boolean): Boolean = p(e)
    final def exists(p: E => Boolean): Boolean = p(e)*/
    private[iteratee] final def normalize: Input[E] = this
    private[iteratee] final def toVector: Vector[E] = Vector(e)
  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](es: Vector[E]): Input[E] = new Input[E] {
    final def foldWith[Z](folder: Folder[E, Z]): Z = folder.onChunk(es)
    final def isEnd: Boolean = false
    final def map[B](f: E => B): Input[B] = chunk(es.map(f(_)))
    final def flatMap[B](f: E => Input[B]): Input[B] = es.tail.foldLeft(f(es.head)) {
      case (acc, _) if acc.isEnd => end
      case (acc, e) =>
        val ei = f(e)
        if (ei.isEnd) end else chunk(acc.toVector ++ ei.toVector)
    }
    /*final def foreach(f: E => Unit): Unit = es.foreach(f(_))
    final def filter(p: E => Boolean): Input[E] = Input.chunk(es.filter(p))
    final def forall(p: E => Boolean): Boolean = es.forall(p(_))
    final def exists(p: E => Boolean): Boolean = es.exists(p(_))*/

    private[iteratee] final def normalize: Input[E] = {
      val c = es.lengthCompare(1)
      if (c == 0) el(es.head) else this
    }

    private[iteratee] final def toVector: Vector[E] = es
  }

  /**
   * We define a single end-of-stream value and cast it to the appropriate type
   * in `end` in order to avoid allocations.
   */
  private[this] final val endValue: Input[Nothing] = new Input[Nothing] {
    final def foldWith[A](folder: Folder[Nothing, A]): A = folder.onEnd
    final val isEnd: Boolean = true
    final val isEmpty: Boolean = false
    final def map[B](f: Nothing => B): Input[B] = this.asInstanceOf[Input[B]]
    final def flatMap[B](f: Nothing => Input[B]): Input[B] = this.asInstanceOf[Input[B]]
    /*final def foreach(f: Nothing => Unit): Unit = ()
    final def filter(p: Nothing => Boolean): Input[Nothing] = this
    final def forall(p: Nothing => Boolean): Boolean = true
    final def exists(p: Nothing => Boolean): Boolean = false*/
    private[iteratee] final val normalize: Input[Nothing] = this
    private[iteratee] final val toVector: Vector[Nothing] = Vector.empty
  }
}
