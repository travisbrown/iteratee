package io.iteratee

import cats.{ Eval, Id }
import cats.free.Trampoline
import cats.std.{ Function0Instances, IntInstances, ListInstances, VectorInstances }
import org.scalacheck.{ Gen, Prop }
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline

class IterateeSuite extends FunSuite with Checkers with Discipline
  with ArbitraryInstances with EqInstances
  with Function0Instances with IntInstances with ListInstances with VectorInstances {

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
      Iteratee.head[Int, Id].process(Enumerator.enumStream(s)) === s.headOption
    }
  }

  test("peek") {
    check { (s: Stream[Int]) =>
      val iteratee = for {
        head <- Iteratee.peek[Int, Id]
        all <- Iteratee.consume[Int, Id]
      } yield (head, all)

      val result = (s.headOption, s.toVector)

      iteratee.process(Enumerator.enumStream(s)) === result
    }
  }

  test("consume") {
    check { (s: Stream[Int]) =>
      Iteratee.consumeIn[Int, Id, List].process(Enumerator.enumStream(s)) === s.toList
    }
  }

  test("length") {
    check { (s: Stream[Int]) =>
      Iteratee.length[Int, Id].process(Enumerator.enumStream(s)) === s.size
    }
  }

  test("contramap") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      Iteratee.sum[Int, Id].contramap((_: Int) + 1).process(Enumerator.enumStream(s)) === result
    }
  }

  test("through") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      val inc = Enumeratee.map[Int, Int, Id]((_: Int) + 1)
      Iteratee.sum[Int, Id].through(inc).process(Enumerator.enumStream(s)) === result
    }
  }

  test("zip") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Int, Eval]
      val len = Iteratee.length[Int, Eval]
      val result = (e.source.map(_.toInt).sum, e.source.size)
      sum.zip(len).process(e.enumerator.map(_.toInt)).value === result
    }
  }

  test("zip with leftovers (scalaz/scalaz#1068)") {
    check {
      Prop.forAll(Gen.posNum[Int], Gen.posNum[Int]) { (x: Int, y: Int) =>
        val takeX = Iteratee.take[Int, Eval](x)
        val takeY = Iteratee.take[Int, Eval](y)
        val size = x * y
        val enum = Enumerator.enumStream[Int, Eval](Stream.from(0).take(size))

        val expected = (0 until size).toVector.grouped(math.max(x, y)).map { vs =>
          (vs.take(x), vs.take(y))
        }.toVector

        takeX.zip(takeY).sequenceI.wrap(enum).drain.value === expected
      }
    }
  }

  test("zip different lengths") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Int, Eval]
      val head = Iteratee.head[Int, Eval]
      val result0 = (e.source.map(_.toInt).sum, e.source.headOption)
      val result1 = result0.swap

      sum.zip(head).process(e.enumerator.map(_.toInt)).value === result0 &&
      head.zip(sum).process(e.enumerator.map(_.toInt)).value === result1
    }
  }

  test("reverse") {
    check { (e: LargeEnumerator[Int]) =>
      val iteratee = Iteratee.reversed[Int, Eval]
      val result = e.source.reverse.toVector
      iteratee.process(e.enumerator).value === result
    }
  }

  test("take") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val iteratee = Iteratee.take[Int, Eval](n)
      val result = e.source.take(n).toVector
      iteratee.process(e.enumerator).value === result
    }
  }

  test("takeWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0)
      val iteratee = Iteratee.takeWhile[Int, Id](_ < n)
      val result = s.takeWhile(_ < n).toVector
      iteratee.process(Enumerator.enumStream(s)) === result &&
      iteratee.process(Enumerator.enumList(s.takeWhile(_ < n).toList)) === result
    }
  }

  test("drop") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val remainder = Iteratee.drop[Int, Eval](n).flatMap(_ => Iteratee.consume[Int, Eval])
      val result = e.source.drop(n).toVector
      remainder.process(e.enumerator).value === result
    }
  }

  test("dropWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0).take(1000)
      val remainder = Iteratee.dropWhile[Int, Eval](_ < n).flatMap(_ => Iteratee.consume[Int, Eval])
      val result = s.dropWhile(_ < n).toVector
      remainder.process(Enumerator.enumStream(s)).value === result &&
      remainder.process(Enumerator.enumList(s.dropWhile(_ < n).toList)).value === result
    }
  }

  test("fold in constant stack space") {
    val iter = Iteratee.fold[Int, Id, Int](0) { case (a, v) => a + v }.up[Trampoline]
    val enum = Enumerator.enumStream[Int, Trampoline](Stream.fill(10000)(1))
    iter.process(enum).run === 10000
  }
}
