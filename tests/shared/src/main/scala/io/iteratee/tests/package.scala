package io.iteratee

import cats.Eq
import org.scalatest.Tag

package tests {
  object NoScala210Test extends Tag("io.iteratee.tests.NoScala210Test")
}

package object tests {
  implicit val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals
}
