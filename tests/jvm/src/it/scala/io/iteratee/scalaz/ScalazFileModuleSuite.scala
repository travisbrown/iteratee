package io.iteratee.scalaz

import cats.Eq
import io.iteratee.tests.files.FileModuleSuite
import scalaz.concurrent.Task

class ScalazFileModuleSuite extends FileModuleSuite[Task] with ScalazModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafePerformSyncAttempt.toEither)
}
