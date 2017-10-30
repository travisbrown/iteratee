package io.iteratee.twitter

import cats.Eq
import com.twitter.conversions.time._
import com.twitter.util.Future
import io.catbird.util.{ futureEqWithFailure, twitterFutureInstance }
import io.iteratee.testing.files.FileModuleSuite

class FutureFileModuleTests extends FileModuleSuite[Future] with FutureModule {
  def monadName: String = "Future"

  implicit def eqF[A: Eq]: Eq[Future[A]] = futureEqWithFailure[A](2.seconds)
}
