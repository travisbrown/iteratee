package io.iteratee

import cats.{ Eval, Id }
import cats.free.Trampoline
import cats.std.{ Function0Instances, IntInstances, ListInstances }
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline

class IterateeSuite extends FunSuite with Checkers with Discipline
  with Function0Instances with IntInstances with ListInstances {

  type EvalEIntIteratee[E] = Iteratee[E, Eval, Vector[Int]]

  checkAll(
    "Iteratee[Vector[Int], Eval, Vector[Int]]",
    ContravariantTests[EvalEIntIteratee].contravariant[Vector[Int], Int, Vector[Int]]
  )

  checkAll(
    "Iteratee[Vector[Int], Eval, Vector[Int]]",
    ContravariantTests[EvalEIntIteratee].invariant[Vector[Int], Int, Vector[Int]]
  )

  test("head") {
    check { (s: Stream[Int]) =>
      Iteratee.head[Int, Id].feedE(Enumerator.enumStream(s)).run === s.headOption
    }
  }

  test("peek") {
    check { (s: Stream[Int]) =>
      val iteratee = for {
        head <- Iteratee.peek[Int, Id]
        all <- Iteratee.consume[Int, Id]
      } yield (head, all)

      val result = (s.headOption, s.toVector)

      iteratee.feedE(Enumerator.enumStream(s)).run === result
    }
  }

  test("consume") {
    check { (s: Stream[Int]) =>
      Iteratee.consumeIn[Int, Id, List].feedE(Enumerator.enumStream(s)).run === s.toList
    }
  }

  test("length") {
    check { (s: Stream[Int]) =>
      Iteratee.length[Int, Id].feedE(Enumerator.enumStream(s)).run === s.size
    }
  }

  test("contramap") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      Iteratee.sum[Int, Id].contramap((_: Int) + 1).feedE(Enumerator.enumStream(s)).run === result
    }
  }

  test("through") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      val inc = Enumeratee.map[Int, Int, Id]((_: Int) + 1)
      Iteratee.sum[Int, Id].through(inc).feedE(Enumerator.enumStream(s)).run === result
    }
  }

  test("zip") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Int, Eval]
      val len = Iteratee.length[Int, Eval]
      val result = (e.source.map(_.toInt).sum, e.source.size)
      sum.zip(len).feedE(e.enumerator.map(_.toInt)).run.value === result
    }
  }

  test("zip different lengths") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Int, Eval]
      val head = Iteratee.head[Int, Eval]
      val result0 = (e.source.map(_.toInt).sum, e.source.headOption)
      val result1 = result0.swap

      sum.zip(head).feedE(e.enumerator.map(_.toInt)).run.value === result0 &&
      head.zip(sum).feedE(e.enumerator.map(_.toInt)).run.value === result1
    }
  }

  test("reverse") {
    check { (e: LargeEnumerator[Int]) =>
      val iteratee = Iteratee.reversed[Int, Eval]
      val result = e.source.reverse.toVector
      iteratee.feedE(e.enumerator).run.value === result
    }
  }

  test("take") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val iteratee = Iteratee.take[Int, Eval](n)
      val result = e.source.take(n).toVector
      iteratee.feedE(e.enumerator).run.value === result
    }
  }

  test("takeWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0)
      val iteratee = Iteratee.takeWhile[Int, Id](_ < n)
      val result = s.takeWhile(_ < n).toVector
      iteratee.feedE(Enumerator.enumStream(s)).run === result &&
      iteratee.feedE(Enumerator.enumList(s.takeWhile(_ < n).toList)).run === result
    }
  }

  test("drop") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val remainder = Iteratee.drop[Int, Eval](n).flatMap(_ => Iteratee.consume[Int, Eval])
      val result = e.source.drop(n).toVector
      remainder.feedE(e.enumerator).run.value === result
    }
  }

  test("dropWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0).take(1000)
      val remainder = Iteratee.dropWhile[Int, Eval](_ < n).flatMap(_ => Iteratee.consume[Int, Eval])
      val result = s.dropWhile(_ < n).toVector
      remainder.feedE(Enumerator.enumStream(s)).run.value === result &&
      remainder.feedE(Enumerator.enumList(s.dropWhile(_ < n).toList)).run.value === result
    }
  }

  test("fold in constant stack space") {
    val iter = Iteratee.fold[Int, Id, Int](0) { case (a, v) => a + v }.up[Trampoline]
    val enum = Enumerator.enumStream[Int, Trampoline](Stream.fill(10000)(1))
    iter.feedE(enum).run.run === 10000
  }
}
