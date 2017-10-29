package io.iteratee

import cats.instances.try_._
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import io.iteratee.tests.TrySuite
import scala.util.Try

class TryEnumerateeTests extends EnumerateeSuite[Try] with TrySuite
class TryEnumeratorTests extends EnumeratorSuite[Try] with TrySuite
class TryIterateeTests extends IterateeErrorSuite[Try, Throwable] with TrySuite
