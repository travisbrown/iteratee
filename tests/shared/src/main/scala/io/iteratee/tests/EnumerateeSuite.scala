package io.iteratee.tests

import cats.Monad
import cats.laws.discipline.{ CategoryTests, ProfunctorTests }
import io.iteratee.Enumeratee
import org.scalacheck.{ Gen, Prop }

abstract class EnumerateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  type EnumerateeF[O, I] = Enumeratee[F, O, I]

  checkAll(s"Enumeratee[$monadName, Int, Int]", ProfunctorTests[EnumerateeF].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll(s"Enumeratee[$monadName, Int, Int]", CategoryTests[EnumerateeF].category[Int, Int, Int, Int])

  test("map") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.mapE(map(_ + 1)).toVector === F.pure(eav.values.map(_ + 1))
    }
  }

  test("mapK") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.mapE(mapK(i => F.pure(i + 1))).toVector === F.pure(eav.values.map(_ + 1))
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

  test("filterK") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val p:  Int => Boolean    = _ % 2 == 0
      val fp: Int => F[Boolean] = i => F.pure(p(i))

      eav.enumerator.mapE(filterK(fp)).toVector === F.pure(eav.values.filter(p))
    }
  }

  test("sequenceI") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.mapE(sequenceI(take(n))).toVector === F.pure(eav.values.grouped(n).toVector)
      }
    }
  }

  test("sequenceI with iteratee that stops early") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.mapE(sequenceI(take(n))).run(head) === F.pure(eav.values.grouped(n).toVector.headOption)
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
}
