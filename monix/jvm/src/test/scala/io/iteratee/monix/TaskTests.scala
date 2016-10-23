package io.iteratee.monix

import cats.Eq
import cats.data.Xor
import io.iteratee.monix.task._
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

trait TaskSuite extends ModuleSuite[Task] with DefaultTaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by { task =>
    Await.result(task.materialize.map(Xor.fromTry).runAsync, 5.seconds)
  }
}

class TaskEnumerateeTests extends EnumerateeSuite[Task] with TaskSuite
class TaskEnumeratorTests extends EnumeratorSuite[Task] with TaskSuite
class TaskIterateeTests extends IterateeErrorSuite[Task, Throwable] with TaskSuite
