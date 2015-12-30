package io.iteratee.internal

import cats.Applicative

/**
 * Represents a pair of functions that can be used to reduce a [[Step]] to a value.
 *
 * Combining two "functions" into a single class allows us to avoid allocations.
 *
 * @tparam E The type of the input data
 * @tparam F The effect type constructor
 * @tparam A The type of the result calculated by the [[Iteratee]]
 * @tparam B The type of the result of the fold
 */
abstract class StepFolder[F[_], E, A, B] extends Serializable {
  def onCont(k: Input[E] => F[Step[F, E, A]]): B
  def onDone(value: A, remainder: Input[E]): B
}

abstract class MapContStepFolder[F[_], E, A](step: Step[F, E, A])(implicit F: Applicative[F])
  extends StepFolder[F, E, A, F[Step[F, E, A]]] {
    final def onDone(value: A, remainder: Input[E]): F[Step[F, E, A]] = F.pure(step)
  }
