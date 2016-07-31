package io.iteratee.monix

import cats.Eq
import cats.data.Xor
import io.iteratee.tests.files.FileModuleSuite
import monix.cats._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

class MonixFileModuleSuite extends FileModuleSuite[Task] with MonixModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by { task =>
    Await.result(task.materialize.map(Xor.fromTry).runAsync, 5.seconds)
  }
}
