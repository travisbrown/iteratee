package io.iteratee.task

import algebra.Eq
import io.iteratee.tests.files.FileModuleSuite
import scalaz.concurrent.Task

class TaskFileModuleSuite extends FileModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.attemptRun.toEither)
}
