package io.iteratee.internal

import cats.Applicative

private[iteratee] abstract class StepInputFolder[F[_], E, A](implicit F: Applicative[F])
  extends Input.Folder[E, Step[F, E, A]] {
  final def next(in: Input[E]): Step[F, E, A] = in.foldWith(this)

  final def onEmpty: Step[F, E, A] = Step.pureCont(next)
}

private[iteratee] abstract class StepUnitInputFolder[F[_], E](implicit F: Applicative[F])
  extends StepInputFolder[F, E, Unit] {
  final def onEnd: Step[F, E, Unit] = StepUnitInputFolder.done.asInstanceOf[Step[F, E, Unit]]
}

private[iteratee] object StepUnitInputFolder {
  final val done: Step[Nothing, Nothing, Unit] = Step.done[Nothing, Nothing, Unit]((), Input.end)
}
