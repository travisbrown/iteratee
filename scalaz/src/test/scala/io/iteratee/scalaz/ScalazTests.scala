package io.iteratee.scalaz

import cats.Eq
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }
import scalaz.concurrent.Task

trait ScalazSuite extends ModuleSuite[Task] with ScalazModule {
  def monadName: String = "Task"

  implicit def eqF[A: Eq]: Eq[Task[A]] = Eq.by(_.unsafePerformSyncAttempt.toEither)
}

class ScalazEnumerateeTests extends EnumerateeSuite[Task] with ScalazSuite

class ScalazEnumeratorTests extends EnumeratorSuite[Task] with ScalazSuite

class ScalazIterateeTests extends IterateeErrorSuite[Task, Throwable] with ScalazSuite
