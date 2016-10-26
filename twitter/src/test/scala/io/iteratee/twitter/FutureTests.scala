package io.iteratee.twitter

import cats.Eq
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import com.twitter.conversions.time._
import com.twitter.util.Future
import io.catbird.util.{ futureEqWithFailure, twitterFutureInstance }
import io.iteratee.tests.{ EnumerateeSuite, IterateeErrorSuite, ModuleSuite, StackSafeEnumeratorSuite, eqThrowable }

trait FutureSuite extends ModuleSuite[Future] with FutureModule {
  def monadName: String = "com.twitter.util.Future"

  implicit def eqF[A: Eq]: Eq[Future[A]] = futureEqWithFailure[A](2.seconds)
}

class FutureEnumerateeTests extends EnumerateeSuite[Future] with FutureSuite
class FutureEnumeratorTests extends StackSafeEnumeratorSuite[Future] with FutureSuite
class FutureIterateeTests extends IterateeErrorSuite[Future, Throwable] with FutureSuite
