package io.iteratee

import cats.Id
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeSuite, PureSuite }

class PureEnumerateeTests extends EnumerateeSuite[Id] with PureSuite
class PureEnumeratorTests extends EnumeratorSuite[Id] with PureSuite
class PureIterateeTests extends IterateeSuite[Id] with PureSuite
