package io.iteratee.scalaz

import cats.Eq
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }
import scalaz.concurrent.Task

trait TaskSuite extends ModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafePerformSyncAttempt.toEither)
}

class ScalazEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite

class ScalazEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite

class ScalazIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
