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

  type EvalEIntIteratee[E] = Iteratee[Eval, E, Vector[Int]]

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
      Iteratee.head[Id, Int].process(Enumerator.enumStream(s)) === s.headOption
    }
  }

  test("peek") {
    check { (s: Stream[Int]) =>
      val iteratee = for {
        head <- Iteratee.peek[Id, Int]
        all <- Iteratee.consume[Id, Int]
      } yield (head, all)

      val result = (s.headOption, s.toVector)

      iteratee.process(Enumerator.enumStream(s)) === result
    }
  }

  test("consume") {
    check { (s: Stream[Int]) =>
      Iteratee.consumeIn[Id, Int, List].process(Enumerator.enumStream(s)) === s.toList
    }
  }

  test("length") {
    check { (s: Stream[Int]) =>
      Iteratee.length[Id, Int].process(Enumerator.enumStream(s)) === s.size
    }
  }

  test("contramap") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      Iteratee.sum[Id, Int].contramap((_: Int) + 1).process(Enumerator.enumStream(s)) === result
    }
  }

  test("through") {
    check { (s: Stream[Int]) =>
      val result = s.sum + s.size
      val inc = Enumeratee.map[Id, Int, Int]((_: Int) + 1)
      Iteratee.sum[Id, Int].through(inc).process(Enumerator.enumStream(s)) === result
    }
  }

  test("zip") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Eval, Int]
      val len = Iteratee.length[Eval, Int]
      val result = (e.source.map(_.toInt).sum, e.source.size)
      sum.zip(len).process(e.enumerator.map(_.toInt)).value === result
    }
  }

  test("zip with leftovers (scalaz/scalaz#1068)") {
    check {
      Prop.forAll(Gen.posNum[Int], Gen.posNum[Int]) { (x: Int, y: Int) =>
        val takeX = Iteratee.take[Eval, Int](x)
        val takeY = Iteratee.take[Eval, Int](y)
        val size = x * y
        val enum = Enumerator.enumStream[Eval, Int](Stream.from(0).take(size))

        val expected = (0 until size).toVector.grouped(math.max(x, y)).map { vs =>
          (vs.take(x), vs.take(y))
        }.toVector

        takeX.zip(takeY).sequenceI.wrap(enum).drain.value === expected
      }
    }
  }

  test("zip different lengths") {
    check { (e: SmallEnumerator[Short]) =>
      val sum = Iteratee.sum[Eval, Int]
      val head = Iteratee.head[Eval, Int]
      val result0 = (e.source.map(_.toInt).sum, e.source.headOption)
      val result1 = result0.swap

      sum.zip(head).process(e.enumerator.map(_.toInt)).value === result0 &&
      head.zip(sum).process(e.enumerator.map(_.toInt)).value === result1
    }
  }

  test("reverse") {
    check { (e: LargeEnumerator[Int]) =>
      val iteratee = Iteratee.reversed[Eval, Int]
      val result = e.source.reverse.toVector
      iteratee.process(e.enumerator).value === result
    }
  }

  test("take") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val iteratee = Iteratee.take[Eval, Int](n)
      val result = e.source.take(n).toVector
      iteratee.process(e.enumerator).value === result
    }
  }

  test("takeWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0)
      val iteratee = Iteratee.takeWhile[Id, Int](_ < n)
      val result = s.takeWhile(_ < n).toVector
      iteratee.process(Enumerator.enumStream(s)) === result &&
      iteratee.process(Enumerator.enumList(s.takeWhile(_ < n).toList)) === result
    }
  }

  test("drop") {
    check { (e: LargeEnumerator[Int], n: Int) =>
      val remainder = Iteratee.drop[Eval, Int](n).flatMap(_ => Iteratee.consume[Eval, Int])
      val result = e.source.drop(n).toVector
      remainder.process(e.enumerator).value === result
    }
  }

  test("dropWhile") {
    check { (b: Byte) =>
      val n = b.toInt
      val s = Stream.from(0).take(1000)
      val remainder = Iteratee.dropWhile[Eval, Int](_ < n).flatMap(_ => Iteratee.consume[Eval, Int])
      val result = s.dropWhile(_ < n).toVector
      remainder.process(Enumerator.enumStream(s)).value === result &&
      remainder.process(Enumerator.enumList(s.dropWhile(_ < n).toList)).value === result
    }
  }

  test("fold in constant stack space") {
    val iter = Iteratee.fold[Id, Int, Int](0) { case (a, v) => a + v }.up[Trampoline]
    val enum = Enumerator.enumStream[Trampoline, Int](Stream.fill(10000)(1))
    iter.process(enum).run === 10000
  }
}
