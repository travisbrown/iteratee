package io.iteratee.twitter

import cats.Eq
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import com.twitter.util.Try
import io.catbird.util.{ twitterTryEq, twitterTryInstance }
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, ModuleSuite }
import io.iteratee.testing.EqInstances.eqThrowable

trait TrySuite extends ModuleSuite[Try] with TryModule {
  def monadName: String = "com.twitter.util.Try"

  implicit def eqF[A: Eq]: Eq[Try[A]] = twitterTryEq[A]
}

class TryEnumerateeTests extends EnumerateeSuite[Try] with TrySuite
class TryEnumeratorTests extends EnumeratorSuite[Try] with TrySuite
class TryIterateeTests extends IterateeErrorSuite[Try, Throwable] with TrySuite
