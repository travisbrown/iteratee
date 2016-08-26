package io.iteratee

import cats.instances.future._
import io.iteratee.tests.{ EnumerateeSuite, FutureSuite, IterateeErrorSuite, StackSafeEnumeratorSuite }
import io.iteratee.tests.FutureSuite.eqThrowableInStdLibFuture
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FutureEnumerateeTests extends EnumerateeSuite[Future] with FutureSuite
class FutureEnumeratorTests extends StackSafeEnumeratorSuite[Future] with FutureSuite
class FutureIterateeTests extends IterateeErrorSuite[Future, Throwable] with FutureSuite
