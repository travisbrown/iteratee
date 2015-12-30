package io.iteratee.internal

/**
 * Represents four functions that can be used to reduce an [[Input]] to a value.
 *
 * Combining four "functions" into a single class allows us to save allocations.
 * `onEmpty` and `onEl` must be consistent with `onChunk`.
 *
 * @tparam E The element type
 * @tparam Z The result type
 */
abstract class InputFolder[@specialized E, Z] extends Serializable {
  def onEnd: Z
  def onEmpty: Z
  def onEl(e: E): Z
  def onChunk(es: Vector[E]): Z
}
