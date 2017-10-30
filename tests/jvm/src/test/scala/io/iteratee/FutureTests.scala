package io.iteratee

import cats.instances.future._
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.testing.{ EnumerateeSuite, IterateeErrorSuite, StackSafeEnumeratorSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import io.iteratee.tests.FutureSuite
import io.iteratee.tests.FutureSuite.arbitraryNonFatalThrowable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FutureEnumerateeTests extends EnumerateeSuite[Future] with FutureSuite
class FutureEnumeratorTests extends StackSafeEnumeratorSuite[Future] with FutureSuite
class FutureIterateeTests extends IterateeErrorSuite[Future, Throwable] with FutureSuite
