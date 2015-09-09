package io.travisbrown.iteratee

import cats.Id
import cats.free.Trampoline
import cats.std.{ Function0Instances, ListInstances }
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class IterateeTSuite extends FunSuite with Checkers with Function0Instances with ListInstances {
  test("head") {
    check { (s: Stream[Int]) =>
      (IterateeT.head[Int, Id] &= EnumeratorT.enumStream(s)).run === s.headOption
    }
  }

  test("consume") {
    check { (s: Stream[Int]) =>
      (IterateeT.consume[Int, Id, List] &= EnumeratorT.enumStream(s)).run === s.toList
    }
  }

  test("fold in constant stack space") {
    val iter = IterateeT.fold[Int, Id, Int](0) { case (a, v) => a + v }.up[Trampoline]
    val enum = EnumeratorT.enumStream[Int, Trampoline](Stream.fill(10000)(1))
    (iter &= enum).run.run === 10000
  }
}
