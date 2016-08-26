package io.iteratee

import cats.instances.option._
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, OptionSuite, IterateeSuite }

class OptionEnumerateeTests extends EnumerateeSuite[Option] with OptionSuite
class OptionEnumeratorTests extends EnumeratorSuite[Option] with OptionSuite
class OptionIterateeTests extends IterateeSuite[Option] with OptionSuite
