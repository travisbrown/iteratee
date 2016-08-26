package io.iteratee

import cats.instances.either._
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, EitherSuite, eqThrowable }

class EitherEnumerateeTests extends EnumerateeSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
class EitherEnumeratorTests extends EnumeratorSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
class EitherIterateeTests extends IterateeErrorSuite[({ type L[x] = Either[Throwable, x] })#L, Throwable]
    with EitherSuite
