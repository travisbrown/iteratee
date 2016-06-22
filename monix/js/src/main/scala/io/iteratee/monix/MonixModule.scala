package io.iteratee.monix

import cats.MonadError
import io.iteratee.{EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module}
import monix.cats.{AllInstances => MonixInstances}
import monix.eval.Task

trait MonixModule extends MonixInstances with Module[Task]
  with EnumerateeModule[Task]
  with EnumeratorErrorModule[Task, Throwable] with IterateeErrorModule[Task, Throwable] {
  final type M[f[_]] = MonadError[f, Throwable]

  final protected val F: MonadError[Task, Throwable] =
    monixMonadErrorInstancesToCats[Task, Throwable]
}
