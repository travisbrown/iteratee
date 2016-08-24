package io.iteratee.twitter

import cats.Eq
import com.twitter.conversions.time._
import com.twitter.util.Future
import io.catbird.util.{ futureEqWithFailure, twitterFutureInstance }
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite, eqThrowable }

trait FutureSuite extends ModuleSuite[Future] with FutureModule {
  def monadName: String = "Future"

  implicit def eqF[A: Eq]: Eq[Future[A]] = futureEqWithFailure[A](2.seconds)
}

class FutureEnumerateeTests extends EnumerateeSuite[Future] with FutureSuite
class FutureEnumeratorTests extends EnumeratorSuite[Future] with FutureSuite
class FutureIterateeTests extends IterateeErrorSuite[Future, Throwable] with FutureSuite
