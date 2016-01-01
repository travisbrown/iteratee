package io.iteratee

import algebra.{ Eq, Semigroup }
import algebra.laws.GroupLaws
import cats.Show
import cats.std.{ IntInstances, OptionInstances }
import cats.laws.discipline.{ MonadTests, SemigroupKTests, TraverseTests }
import io.iteratee.internal.Input
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class InputSuite extends FunSuite with Discipline with IntInstances with OptionInstances
  with ArbitraryInstances {
  /*checkAll("Input[Int]", GroupLaws[Input[Int]].semigroup)
  checkAll("Input[Int]", MonadTests[Input].monad[Int, Int, Int])
  checkAll("Input[Int]", TraverseTests[Input].traverse[Int, Int, Int, Int, Option, Option])

  test("Show[Input[Int]]") {
    def show(in: Input[Int]): String = Show[Input[Int]].show(in)

    show(Input.empty[Int]) === "empty" &&
    show(Input.el(0)) === "el(0)" &&
    show(Input.chunk(Vector(0, 1))) === "chunk(0, 1)" &&
    show(Input.end[Int]) === "end"
  }*/
}
