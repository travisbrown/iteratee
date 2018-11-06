package io.iteratee.files

import cats.effect.IO
import cats.effect.laws.util.{ TestContext, TestInstances }
import cats.kernel.Eq
import io.iteratee.files.modules.IOModule
import io.iteratee.testing.{ EnumerateeSuite, IterateeErrorSuite, StackSafeEnumeratorSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import io.iteratee.testing.files.FileModuleSuite

trait IOSuite extends IOModule {
  def monadName: String = "IO"

  private[this] lazy val context: TestContext = TestContext()

  implicit def eqF[A](implicit A: Eq[A]): Eq[IO[A]] = TestInstances.eqIO(A, context)
}

class IOFileTests extends FileModuleSuite[IO] with IOSuite
class IOEnumerateeTests extends EnumerateeSuite[IO] with IOSuite
class IOEnumeratorTests extends StackSafeEnumeratorSuite[IO] with IOSuite
class IOIterateeTests extends IterateeErrorSuite[IO, Throwable] with IOSuite
