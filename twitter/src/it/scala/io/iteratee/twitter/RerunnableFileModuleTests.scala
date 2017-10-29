package io.iteratee.twitter

import cats.Eq
import com.twitter.conversions.time._
import io.catbird.util.Rerunnable
import io.iteratee.testing.files.FileModuleSuite

class RerunnableFileModuleTests extends FileModuleSuite[Rerunnable] with RerunnableModule {
  def monadName: String = "Rerunnable"

  implicit def eqF[A: Eq]: Eq[Rerunnable[A]] = Rerunnable.rerunnableEqWithFailure[A](2.seconds)
}
