package io.iteratee

import cats.instances.option._
import io.iteratee.testing.{ EnumerateeSuite, EnumeratorSuite, IterateeSuite }
import io.iteratee.tests.OptionSuite

class OptionEnumerateeTests extends EnumerateeSuite[Option] with OptionSuite
class OptionEnumeratorTests extends EnumeratorSuite[Option] with OptionSuite
class OptionIterateeTests extends IterateeSuite[Option] with OptionSuite
