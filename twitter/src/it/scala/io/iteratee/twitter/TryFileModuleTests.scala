package io.iteratee.twitter

import cats.Eq
import com.twitter.util.Try
import io.catbird.util.{ twitterTryEq, twitterTryInstance }
import io.iteratee.testing.files.FileModuleSuite

class TryFileModuleTests extends FileModuleSuite[Try] with TryModule {
  def monadName: String = "Try"

  implicit def eqF[A: Eq]: Eq[Try[A]] = twitterTryEq[A]
}
