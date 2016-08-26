package io.iteratee

import cats.Id
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IdSuite, IterateeSuite }

class IdEnumerateeTests extends EnumerateeSuite[Id] with IdSuite
class IdEnumeratorTests extends EnumeratorSuite[Id] with IdSuite
class IdIterateeTests extends IterateeSuite[Id] with IdSuite
