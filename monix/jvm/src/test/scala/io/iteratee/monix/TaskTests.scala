package io.iteratee.monix

import cats.Eq
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.monix.task._
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

trait TaskSuite extends ModuleSuite[Task] with DefaultTaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by { task =>
    Await.result(task.materialize.runAsync, 5.seconds)
  }
}

class TaskEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite
class TaskEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite
class TaskIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
