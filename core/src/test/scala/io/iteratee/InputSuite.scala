package io.iteratee

import cats.Show
import cats.std.{ IntInstances, OptionInstances }
import io.iteratee.internal.Input
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class InputSuite extends FunSuite with Discipline with IntInstances with OptionInstances
  with ArbitraryInstances {
  test("Show[Input[Int]]") {
    def show(in: Input[Int]): String = Show[Input[Int]].show(in)

    show(Input.el(0)) === "el(0)" &&
    show(Input.chunk(0, 1, Vector(2, 3))) === "chunk(0, 1, 2, 3)" &&
    show(Input.end[Int]) === "end"
  }
}
