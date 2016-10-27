package io.iteratee

import cats.instances.try_._
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, TrySuite, eqThrowable }
import scala.util.Try

class TryEnumerateeTests extends EnumerateeSuite[Try] with TrySuite
class TryEnumeratorTests extends EnumeratorSuite[Try] with TrySuite
class TryIterateeTests extends IterateeErrorSuite[Try, Throwable] with TrySuite
