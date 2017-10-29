package io.iteratee.scalaz

import cats.Eq
import io.iteratee.testing.files.FileModuleSuite
import scalaz.concurrent.Task

class TaskFileModuleTests extends FileModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafePerformSyncAttempt.toEither)
}
