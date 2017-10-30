package io.iteratee

import cats.instances.either._
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import io.iteratee.tests.EitherSuite

class EitherEnumerateeTests extends EnumerateeSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
class EitherEnumeratorTests extends EnumeratorSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
class EitherIterateeTests extends IterateeErrorSuite[({ type L[x] = Either[Throwable, x] })#L, Throwable]
    with EitherSuite
