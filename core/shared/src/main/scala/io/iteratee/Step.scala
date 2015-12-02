package io.iteratee

import cats.Applicative

/**
 * Represents a pair of functions that can be used to reduce a [[Step]] to a value.
 *
 * Combining two "functions" into a single class allows us to save allocations.
 *
 * @tparam E The type of the input data
 * @tparam F The effect type constructor
 * @tparam A The type of the result calculated by the [[Iteratee]]
 * @tparam B The type of the result of the fold
 */
abstract class StepFolder[E, F[_], A, B] {
  def onCont(k: Input[E] => Iteratee[E, F, A]): B
  def onDone(value: A, remainder: Input[E]): B
}

abstract class MapContStepFolder[E, F[_]: Applicative, A](step: Step[E, F, A])
  extends StepFolder[E, F, A, Iteratee[E, F, A]] {
    def onDone(value: A, remainder: Input[E]): Iteratee[E, F, A] = step.pointI
  }

/**
 * Represents the current state of an [[Iteratee]].
 *
 * An [[Iteratee]] has either already calculated a result ([[Step.done]]) or is waiting for more
 * data ([[Step.cont]]).
 *
 * @tparam E The type of the input data
 * @tparam F The effect type constructor
 * @tparam A The type of the result calculated by the [[Iteratee]]
 */
sealed abstract class Step[E, F[_], A] {
  /**
   * The [[Iteratee]]'s result.
   *
   * In some cases we know that an iteratee has been constructed in such a way that it must be in a
   * completed state, even though that's not tracked by the type system. This method provides
   * (unsafe) access to the result for use in these situations.
   */
  private[iteratee] def unsafeValue: A

  /**
   * Reduce this [[Step]] to a value using the given pair of functions.
   */
  def foldWith[B](folder: StepFolder[E, F, A, B]): B

  def isDone: Boolean

  /**
   * Create an [[Iteratee]] with this [[Step]] as its state.
   */
  def pointI(implicit F: Applicative[F]): Iteratee[E, F, A] = new Iteratee(F.pure(this))
}

object Step {
  /**
   * Create an incomplete state that will use the given function to process the next input.
   */
  def cont[E, F[_], A](k: Input[E] => Iteratee[E, F, A]): Step[E, F, A] =
    new Step[E, F, A] {
      private[iteratee] def unsafeValue: A = Iteratee.diverge[A]
      def isDone: Boolean = false
      def foldWith[B](folder: StepFolder[E, F, A, B]): B = folder.onCont(k)
    }

  /**
   * Create a new completed state with the given result and leftover input.
   */
  def done[E, F[_], A](value: A, remaining: Input[E]): Step[E, F, A] =
    new Step[E, F, A] {
      private[iteratee] def unsafeValue: A = value
      def isDone: Boolean = true
      def foldWith[B](folder: StepFolder[E, F, A, B]): B = folder.onDone(value, remaining)
    }
}
