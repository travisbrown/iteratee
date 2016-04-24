package io.iteratee.tests

import cats.Monad
import cats.laws.discipline.{ CategoryTests, ProfunctorTests }
import io.iteratee.{ Enumeratee, Module }
import org.scalacheck.{ Gen, Prop }
import org.scalacheck.Prop.BooleanOperators

abstract class EnumerateeSuite[F[_]: Monad] extends ModuleSuite[F] { this: Module[F] =>
  type EnumerateeF[O, I] = Enumeratee[F, O, I]

  checkAll(s"Enumeratee[$monadName, Int, Int]", ProfunctorTests[EnumerateeF].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll(s"Enumeratee[$monadName, Int, Int]", CategoryTests[EnumerateeF].category[Int, Int, Int, Int])

  test("map") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.mapE(map(_ + 1)).toVector === F.pure(eav.values.map(_ + 1))
    }
  }

  test("flatMapM") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.mapE(flatMapM(i => F.pure(i + 1))).toVector === F.pure(eav.values.map(_ + 1))
    }
  }

  test("flatMap") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.mapE(flatMap(v => enumVector(Vector(v, v))))

      enumerator.toVector === F.pure(eav.values.flatMap(v => Vector(v, v)))
    }
  }

  test("flatMap with iteratee that stops early") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.mapE(flatMap(v => enumVector(Vector(v, v))))

      enumerator.run(head) === F.pure(eav.values.flatMap(v => Vector(v, v)).headOption)
    }
  }

  test("take") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      (n != Int.MaxValue) ==> {
        val expected = F.pure((eav.values.take(n), eav.values.drop(n)))

        eav.resultWithLeftovers(consume[Int].through(take(n.toLong))) === expected
      }
    }
  }

  test("take that ends mid-chunk") {
    check { (v: Vector[Int]) =>
      enumVector(v).mapE(take(v.size.toLong - 1L)).toVector === F.pure(v.dropRight(1))
    }
  }

  test("take with more than Int.MaxValue values") {
    check { (n: Int) =>
      val items = Vector.fill(1000000)(())
      val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
      val enumerator = repeat(()).flatMap(_ => enumVector(items)).mapE(take(totalSize))

      enumerator.run(length) === F.pure(totalSize)
    }
  }

  test("take with wrap") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      (n != Int.MaxValue) ==> {
        val eavNew = eav.copy(enumerator = take[Int](n.toLong).wrap(eav.enumerator))
        eavNew.resultWithLeftovers(consume) === F.pure((eav.values.take(n), Vector.empty))
      }
    }
  }

  test("takeWhile") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      eav.resultWithLeftovers(consume[Int].through(takeWhile(_ < n))) === F.pure(eav.values.span(_ < n))
    }
  }

  test("takeWhile with wrap") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      val eavNew = eav.copy(enumerator = takeWhile[Int](_ < n).wrap(eav.enumerator))
      eavNew.resultWithLeftovers(consume) === F.pure((eav.values.takeWhile(_ < n), Vector.empty))
    }
  }

  test("takeWhile that ends mid-chunk") {
    check { (n: Byte) =>
      val v = (0 to n.toInt).toVector
      enumVector(v).mapE(takeWhile(_ < n.toInt)).toVector === F.pure(v.dropRight(1))
    }
  }

  test("drop") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      eav.resultWithLeftovers(consume[Int].through(drop(n.toLong))) === F.pure((eav.values.drop(n), Vector.empty))
    }
  }

  test("drop with one left over") {
    check { (v: Vector[Int]) =>
      enumVector(v).mapE(drop(v.size.toLong - 1L)).toVector === F.pure(v.lastOption.toVector)
    }
  }

  test("drop with more than Int.MaxValue values") {
    check { (n: Int) =>
      val items = Vector.fill(1000000)(())
      val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
      val enumerator = repeat(()).flatMap(_ => enumVector(items)).mapE(drop(totalSize))

      enumerator.run(head) === F.pure(Some(()))
    }
  }

  test("dropWhile") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      eav.resultWithLeftovers(consume[Int].through(dropWhile(_ < n))) ===
        F.pure((eav.values.dropWhile(_ < n), Vector.empty))
    }
  }

  test("dropWhile with one left over") {
    check { (n: Byte) =>
      val v = (0 to n.toInt).toVector
      enumVector(v).mapE(dropWhile(_ < n.toInt)).toVector === F.pure(v.lastOption.toVector)
    }
  }

  /**
   * We skip this test on Scala 2.10 because of weird "Bad invokespecial instruction" exceptions
   * that I wasn't able to reproduce in other contexts.
   */
  test("collect", NoScala210Test) {
    check { (eav: EnumeratorAndValues[Int]) =>
      val pf: PartialFunction[Int, Int] = {
        case v if v % 2 == 0 => v + 1
      }

      eav.enumerator.mapE(collect(pf)).toVector === F.pure(eav.values.collect(pf))
    }
  }

  test("filter") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val p: Int => Boolean = _ % 2 == 0

      eav.enumerator.mapE(filter(p)).toVector === F.pure(eav.values.filter(p))
    }
  }

  test("filterM") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val p:  Int => Boolean    = _ % 2 == 0
      val fp: Int => F[Boolean] = i => F.pure(p(i))

      eav.enumerator.mapE(filterM(fp)).toVector === F.pure(eav.values.filter(p))
    }
  }

  test("sequenceI") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.mapE(sequenceI(takeI(n))).toVector === F.pure(eav.values.grouped(n).toVector)
      }
    }
  }

  test("sequenceI with iteratee that stops early") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.mapE(sequenceI(takeI(n))).run(head) === F.pure(eav.values.grouped(n).toVector.headOption)
      }
    }
  }

  test("uniq") {
    check { (xs: Vector[Int]) =>
      val sorted = xs.sorted

      enumVector(sorted).mapE(uniq).toVector === F.pure(sorted.distinct)
    }
  }

  test("uniq with iteratee that stops early") {
    check { (xs: Vector[Int]) =>
      val sorted = xs.sorted

      enumVector(sorted).mapE(uniq).run(head) === F.pure(sorted.distinct.headOption)
    }
  }

  test("uniq with known duplicates") {
    val enumerator = enumVector(Vector(1, 2, 3, 4))
      .append(enumVector(Vector(4, 5, 6, 7)))
      .append(enumOne(7))
      .append(enumOne(8))
      .append(enumVector(Vector(8, 8, 8)))
      .append(enumVector(Vector(9, 10)))
    val result = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(enumerator.mapE(uniq).toVector === F.pure(result))
  }

  test("zipWithIndex") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = eav.values.zipWithIndex.map {
        case (v, i) => (v, i.toLong)
      }

      eav.enumerator.mapE(zipWithIndex).toVector === F.pure(result)
    }
  }

  test("zipWithIndex with iteratee that stops early") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = eav.values.zipWithIndex.map {
        case (v, i) => (v, i.toLong)
      }

      eav.enumerator.mapE(zipWithIndex).run(head) === F.pure(result.headOption)
    }
  }

  test("grouped") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.mapE(grouped(n)).toVector === F.pure(eav.values.grouped(n).toVector)
      }
    }
  }

  test("splitOn") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val p: Int => Boolean = _ % 2 == 0

      def splitOnEvens(xs: Vector[Int]): Vector[Vector[Int]] = if (xs.isEmpty) Vector.empty else {
        val (before, after) = xs.span(x => !p(x))

        before +: splitOnEvens(after.drop(1))
      }

      eav.enumerator.mapE(splitOn(p)).toVector === F.pure(splitOnEvens(eav.values))
    }
  }

  test("cross") {
    check { (eav1: EnumeratorAndValues[Int], eav2: EnumeratorAndValues[Int]) =>
      val result = for {
        v1 <- eav1.values
        v2 <- eav2.values
      } yield (v1, v2)

      eav1.enumerator.mapE(cross(eav2.enumerator)).toVector === F.pure(result)
    }
  }

  test("intersperse") {
    check { (eav: EnumeratorAndValues[Int], delim: Int) =>
      val expected = eav.values.zip(Stream.continually(delim)).flatMap {
        case (x, y) => Vector(x, y)
      }.dropRight(1)

      eav.resultWithLeftovers(consume[Int].through(intersperse(delim))) === F.pure((expected, Vector.empty))
    }
  }
}
