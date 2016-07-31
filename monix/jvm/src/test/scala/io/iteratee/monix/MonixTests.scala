package io.iteratee.monix

import cats.Eq
import cats.data.Xor
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }
import monix.cats._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

trait MonixSuite extends ModuleSuite[Task] with MonixModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by { task =>
    Await.result(task.materialize.map(Xor.fromTry).runAsync, 5.seconds)
  }
}

class MonixEnumerateeTests extends EnumerateeSuite[Task] with MonixSuite

class MonixEnumeratorTests extends EnumeratorSuite[Task] with MonixSuite

class MonixIterateeTests extends IterateeErrorSuite[Task, Throwable] with MonixSuite
