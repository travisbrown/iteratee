package io.travisbrown.iteratee

import algebra.laws.GroupLaws
import cats.Id
import cats.free.Trampoline
import cats.laws.discipline.MonadTests
import cats.std.{ Function0Instances, IntInstances, ListInstances, OptionInstances }
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class EnumeratorTSuite extends FunSuite with Discipline with Function0Instances with IntInstances with ListInstances with OptionInstances {
  checkAll("EnumeratorT[Int, Option]", GroupLaws[EnumeratorT[Int, Option]].monoid)
  checkAll("EnumeratorT[Int, Option]", MonadTests[EnumeratorT[?, Option]].monad[Int, Int, Int])

  case class ShortList[A](xs: List[A])

  object ShortList {
    implicit def arbitraryShortList[A](implicit A: Arbitrary[A]): Arbitrary[ShortList[A]] =
      Arbitrary(Gen.containerOfN[List, A](10, A.arbitrary).map(ShortList(_)))
  }


  test("eof") {
    val enum = EnumeratorT.enumEofT[Int, Id]
    (IterateeT.consume[Int, Id, List] &= enum).run === Nil
  }

  test("map") {
    check { (s: Stream[Int], i: Int) =>
      val enum = EnumeratorT.enumStream[Int, Id](s)
      (IterateeT.consume[Int, Id, List] &= enum.map(_ * i)).run === s.map(_ * i)
    }
  }

  test("flatMap") {
    check { (s: ShortList[Int]) =>
      val enum = EnumeratorT.enumList[Int, Id](s.xs)
      val result = s.xs.flatMap(i => s.xs.map(_ + i))
      (IterateeT.consume[Int, Id, List] &= enum.flatMap(i => enum.map(_ + i))).run === result
    }
  }

  test("flatten in a generalized fashion") {
    check { (s: List[Int]) =>
      val enum = EnumeratorT.enumOne[List[Int], List](s)
      (IterateeT.consume[Int, List, List] &= enum.flatten).run.flatten === s
    }
  }

  test("uniq") {
    val enum = EnumeratorT.enumStream[Int, Id](Stream(1, 1, 2, 2, 2, 3, 3))
    (IterateeT.consume[Int, Id, List] &= enum.uniq).run === List(1, 2, 3)
  }

  test("zipWithIndex") {
    val enum = EnumeratorT.enumStream[Int, Id](Stream(3, 4, 5))
    val result = List((3, 0L), (4, 1L), (5, 2L))
    (IterateeT.consume[(Int, Long), Id, List] &= enum.zipWithIndex).run === result
  }

  test("zipWithIndex in combination with another function") {
    val enum = EnumeratorT.enumStream[Int, Id](Stream(3, 4, 4, 5))
    val result = List((3, 0L), (4, 1L), (5, 2L))
    (IterateeT.consume[(Int, Long), Id, List] &= enum.uniq.zipWithIndex).run === result
  }

  test("lift") {
    val enum = EnumeratorT.liftM(List(1, 2, 3))
    (IterateeT.collectT[Int, List, Id] &= enum.map(_ * 2)).run === List(2, 4, 6)
  }

  test("enumerate an array") {
    val enum = EnumeratorT.enumArray[Int, Id](Array(1, 2, 3, 4, 5), 0, Some(3))
    (IterateeT.consume[Int, Id, List] &= enum).run === List(1, 2, 3)
  }

  test("drain") {
    check { (s: Stream[Int]) =>
      val enum = EnumeratorT.enumStream[Int, Id](s)
      enum.drainTo[List] === s.toList
    }
  }
}
