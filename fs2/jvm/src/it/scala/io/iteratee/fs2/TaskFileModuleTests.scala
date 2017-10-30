package io.iteratee.fs2

import cats.Eq
import fs2.Task
import io.iteratee.fs2.task._
import io.iteratee.testing.files.FileModuleSuite

class TaskFileModuleTests extends FileModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafeAttemptValue)
}
