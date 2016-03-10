package io.iteratee.task

import algebra.Eq
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }
import scalaz.concurrent.Task

trait TaskSuite { this: ModuleSuite[Task] =>
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.attemptRun.toEither)
}

class TaskEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite

class TaskEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite

class TaskIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
