package io.iteratee.monix

import cats.Eq
import io.iteratee.monix.task._
import io.iteratee.testing.files.FileModuleSuite
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

class TaskFileModuleTests extends FileModuleSuite[Task] with DefaultTaskModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by { task =>
    Await.result(task.materialize.runAsync, 5.seconds)
  }
}
