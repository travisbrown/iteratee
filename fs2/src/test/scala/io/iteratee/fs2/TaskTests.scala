package io.iteratee.fs2

import cats.Eq
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import fs2.Task
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }

trait TaskSuite extends ModuleSuite[Task] with TaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafeAttemptValue)
}

class TaskEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite
class TaskEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite
class TaskIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
