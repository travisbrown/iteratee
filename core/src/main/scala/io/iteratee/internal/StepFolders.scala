package io.iteratee.internal

import cats.Applicative

abstract class MapContStepFolder[F[_], E, A](step: Step[F, E, A])(implicit F: Applicative[F])
  extends Step.Folder[F, E, A, F[Step[F, E, A]]] {
    final def onDone(value: A): F[Step[F, E, A]] = F.pure(step)
  }
