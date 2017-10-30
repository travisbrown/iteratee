package io.iteratee.scalaz

import cats.Eq
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import scalaz.concurrent.Task

trait TaskSuite extends ModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafePerformSyncAttempt.toEither)
}

class TaskEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite
class TaskEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite
class TaskIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
