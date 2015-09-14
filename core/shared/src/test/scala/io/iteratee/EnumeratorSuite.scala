package io.iteratee

import algebra.Semigroup
import algebra.laws.GroupLaws
import cats.{ Eval, Id }
import cats.free.Trampoline
import cats.laws.discipline.MonadTests
import cats.std.{ Function0Instances, IntInstances, ListInstances, OptionInstances }
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class EnumeratorSuite extends FunSuite with Discipline
  with Function0Instances with IntInstances with ListInstances with OptionInstances
  with ArbitraryKInstances with EqInstances {
  type OptionEnumerator[E] = Enumerator[E, Option]

  checkAll("Enumerator[Int, Option]", GroupLaws[Enumerator[Int, Option]].monoid)
  checkAll("Enumerator[Int, Option]", MonadTests[OptionEnumerator].monad[Int, Int, Int])

  case class ShortList[A](xs: List[A])

  object ShortList {
    implicit def arbitraryShortList[A](implicit A: Arbitrary[A]): Arbitrary[ShortList[A]] =
      Arbitrary(Gen.containerOfN[List, A](10, A.arbitrary).map(ShortList(_)))
  }

  test("eof") {
    val enum = Enumerator.enumEof[Int, Id]
    (Iteratee.consumeIn[Int, Id, List].feedE(enum)).run === Nil
  }

  test("map") {
    check { (s: Stream[Int], i: Int) =>
      val enum = Enumerator.enumStream[Int, Id](s)
      Iteratee.consumeIn[Int, Id, List].feedE(enum.map(_ * i)).run === s.map(_ * i)
    }
  }

  test("flatMap") {
    check { (s: ShortList[Int]) =>
      val enum = Enumerator.enumList[Int, Id](s.xs)
      val result = s.xs.flatMap(i => s.xs.map(_ + i))
      Iteratee.consumeIn[Int, Id, List].feedE(enum.flatMap(i => enum.map(_ + i))).run === result
    }
  }

  test("flatten in a generalized fashion") {
    check { (s: List[Int]) =>
      val enum = Enumerator.enumOne[List[Int], List](s)
      Iteratee.consumeIn[Int, List, List].feedE(enum.flatten).run.flatten === s
    }
  }

  test("collect") {
    check { (e: LargeEnumerator[Int]) =>
      def p(i: Int): Boolean = i % 2 == 0

      val enumerator = e.enumerator.collect {
        case i if p(i) => i
      }

      val result = e.source.collect {
        case i if p(i) => i
      }.toVector

      enumerator.drainTo.value === result
    }
  }

  test("filter") {
    check { (e: LargeEnumerator[Int]) =>
      def p(i: Int): Boolean = i % 2 == 0

      val enumerator = e.enumerator.filter(p)
      val result = e.source.filter(p).toVector

      enumerator.drainTo.value === result
    }
  }

  test("uniq") {
    val enum = Enumerator.enumStream[Int, Id](Stream(1, 1, 2, 2, 2, 3, 3))
    Iteratee.consumeIn[Int, Id, List].feedE(enum.uniq).run === List(1, 2, 3)
  }

  test("zipWithIndex") {
    check { (e: LargeEnumerator[Int]) =>
      val enumerator = e.enumerator.zipWithIndex
      val result = e.source.zipWithIndex.toVector

      enumerator.drainTo.value === result
    }
  }

  test("zipWithIndex in combination with another function") {
    val enum = Enumerator.enumStream[Int, Id](Stream(3, 4, 4, 5))
    val result = List((3, 0L), (4, 1L), (5, 2L))
    Iteratee.consumeIn[(Int, Long), Id, List].feedE(enum.uniq.zipWithIndex).run === result
  }

  test("grouped") {
    check { (e: LargeEnumerator[Int]) =>
      val size = 4

      val enumerator = e.enumerator.grouped(size)
      val result = e.source.grouped(size).map(_.toVector).toVector

      enumerator.drainTo.value === result
    }
  }

  test("splitOn") {
    def splitOnTrue(xs: Stream[Boolean], current: Vector[Boolean]): Vector[Vector[Boolean]] =
      xs match {
        case true #:: t => current +: splitOnTrue(t, Vector.empty)
        case false #:: t => splitOnTrue(t, current :+ false)
        case Stream.Empty => if (current.isEmpty) Vector.empty else Vector(current)
      }

    check { (e: LargeEnumerator[Boolean]) =>
      val enumerator = e.enumerator.splitOn(identity)
      val result = splitOnTrue(e.source, Vector.empty)

      enumerator.drainTo.value === result
    }
  }

  test("liftM") {
    val enum = Enumerator.liftM(List(1, 2, 3))
    Iteratee.collectT[Int, List, Id].feedE(enum.map(_ * 2)).run === List(2, 4, 6)
  }

  test("empty") {
    val enumerator = Enumerator.empty[Int, Id]
    enumerator.drainTo === Vector.empty[Int]
  }

  test("enumerate an array") {
    val enum = Enumerator.enumArray[Int, Id](Array(1, 2, 3, 4, 5), 0, Some(3))
    Iteratee.consumeIn[Int, Id, List].feedE(enum).run === List(1, 2, 3)
  }

  test("drain") {
    check { (s: Stream[Int]) =>
      val enum = Enumerator.enumStream[Int, Id](s)
      enum.drainToF[List] === s.toList
    }
  }

  test("reduced") {
    check { (s: SmallEnumerator[Int]) =>
      val enumerator = s.enumerator.reduced(Vector.empty[Int])(_ :+ _)
      val result = Vector(s.source.toVector)

      enumerator.drainTo.value === result
    }
  }

  test("cross") {
    check { (s: SmallEnumerator[Int], t: SmallEnumerator[String]) =>
      val enumerator = s.enumerator.cross(t.enumerator)
      val result = (
        for {
          sv <- s.source
          tv <- t.source
        } yield (sv, tv)
      ).toVector

      enumerator.drainTo.value === result
    }
  }

  test("perform") {
    check { (e: SmallEnumerator[Int]) =>
      var marker = false
      val action = Eval.later(marker = true)

      val enumerator = Semigroup[Enumerator[Int, Eval]].combine(
        e.enumerator,
        Enumerator.perform[Int, Eval, Unit](action)
      )

      enumerator.drainTo.value
      
      marker === true
    }
  }

  test("repeat") {
    check { (i: Int, count: Short) =>
      val enumerator = Enumerator.repeat[Int, Eval](i)
      val result = Vector.fill(count.toInt)(i)

      Iteratee.take[Int, Eval](count.toInt).feedE(enumerator).run.value === result
    }
  }

  test("iterate") {
    check { (i: Short, count: Short) =>
      val enumerator = Enumerator.iterate[Int, Eval](_ + 1, i.toInt)
      val result = Vector.iterate(i.toInt, count.toInt)(_ + 1)

      Iteratee.take[Int, Eval](count.toInt).feedE(enumerator).run.value === result
    }
  }
}
