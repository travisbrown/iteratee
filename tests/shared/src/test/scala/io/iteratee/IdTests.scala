package io.iteratee

import cats.Id
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeSuite }
import io.iteratee.tests.IdSuite

class IdEnumerateeTests extends EnumerateeSuite[Id] with IdSuite
class IdEnumeratorTests extends EnumeratorSuite[Id] with IdSuite
class IdIterateeTests extends IterateeSuite[Id] with IdSuite
