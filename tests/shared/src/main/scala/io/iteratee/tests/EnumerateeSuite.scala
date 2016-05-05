package io.iteratee.tests

import cats.Monad
import cats.laws.discipline.{ CategoryTests, ProfunctorTests }
import io.iteratee.{ Enumeratee, EnumerateeModule, EnumeratorModule, IterateeModule, Module }
import org.scalacheck.{ Arbitrary, Gen }

abstract class EnumerateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  this: Module[F] with EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] =>

  type EnumerateeF[O, I] = Enumeratee[F, O, I]

  checkLaws(s"Enumeratee[$monadName, Int, Int]", ProfunctorTests[EnumerateeF].profunctor[Int, Int, Int, Int, Int, Int])
  checkLaws(s"Enumeratee[$monadName, Int, Int]", CategoryTests[EnumerateeF].category[Int, Int, Int, Int])

  "map" should "transform the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.mapE(map(_ + 1)).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMapM" should "transform the stream with a pure effectful function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.mapE(flatMapM(i => F.pure(i + 1))).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMap" should "transform the stream with a function into enumerators" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.mapE(flatMap(v => enumVector(Vector(v, v))))

      assert(enumerator.toVector === F.pure(eav.values.flatMap(v => Vector(v, v))))
    }
  }

  it should "work with an iteratee that stops early" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val enumerator = eav.enumerator.mapE(flatMap(v => enumVector(Vector(v, v))))

    assert(enumerator.run(head) === F.pure(eav.values.flatMap(v => Vector(v, v)).headOption))
  }

  "take" should "consume the specified number of values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    /**
     * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
     * ScalaCheck is likely to run into.
     */
    whenever(n != Int.MaxValue) {
      val expected = F.pure((eav.values.take(n), eav.values.drop(n)))

      assert(eav.resultWithLeftovers(consume[Int].through(take(n.toLong))) === expected)
    }
  }

  it should "work when it ends mid-chunk" in forAll { (v: Vector[Int]) =>
    assert(enumVector(v).mapE(take(v.size.toLong - 1L)).toVector === F.pure(v.dropRight(1)))
  }

  it should "work with wrap" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    /**
      * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
     * ScalaCheck is likely to run into.
     */
    whenever(n != Int.MaxValue) {
      val eavNew = eav.copy(enumerator = take[Int](n.toLong).wrap(eav.enumerator))

      assert(eavNew.resultWithLeftovers(consume) === F.pure((eav.values.take(n), Vector.empty)))
    }
  }

  "takeWhile" should "consume the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    assert(eav.resultWithLeftovers(consume[Int].through(takeWhile(_ < n))) === F.pure(eav.values.span(_ < n)))
  }

  it should "work with wrap" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val eavNew = eav.copy(enumerator = takeWhile[Int](_ < n).wrap(eav.enumerator))

    assert(eavNew.resultWithLeftovers(consume) === F.pure((eav.values.takeWhile(_ < n), Vector.empty)))
  }

  it should "work when it ends mid-chunk" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).mapE(takeWhile(_ < n.toInt)).toVector === F.pure(v.dropRight(1)))
  }

  "drop" should "drop the specified number of values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    assert(eav.resultWithLeftovers(consume[Int].through(drop(n.toLong))) === F.pure((eav.values.drop(n), Vector.empty)))
  }

  it should "work with one left over" in forAll { (v: Vector[Int]) =>
    assert(enumVector(v).mapE(drop(v.size.toLong - 1L)).toVector === F.pure(v.lastOption.toVector))
  }

  it should "work with more than Int.MaxValue values" in forAll { (n: Int) =>
    val items = Vector.fill(1000000)(())
    val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
    val enumerator = repeat(()).flatMap(_ => enumVector(items)).mapE(drop(totalSize))

    assert(enumerator.run(head) === F.pure(Some(())))
  }

  "dropWhile" should "drop the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val expected = F.pure((eav.values.dropWhile(_ < n), Vector.empty[Int]))

    assert(eav.resultWithLeftovers(consume[Int].through(dropWhile(_ < n))) === expected)
  }

  it should "work with one left over" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).mapE(dropWhile(_ < n.toInt)).toVector === F.pure(v.lastOption.toVector))
  }

  "dropWhileM" should "drop the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val expected = F.pure((eav.values.dropWhile(_ < n), Vector.empty[Int]))

    assert(eav.resultWithLeftovers(consume[Int].through(dropWhileM(i => F.pure(i < n)))) === expected)
  }

  it should "work with one left over" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).mapE(dropWhileM(i => F.pure(i < n.toInt))).toVector === F.pure(v.lastOption.toVector))
  }

  /**
   * We skip this test on Scala 2.10 because of weird "Bad invokespecial instruction" exceptions
   * that I wasn't able to reproduce in other contexts.
   */
  "collect" should "filter the stream using a partial function" taggedAs(NoScala210Test) in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val pf: PartialFunction[Int, Int] = {
        case v if v % 2 == 0 => v + 1
      }

      assert(eav.enumerator.mapE(collect(pf)).toVector === F.pure(eav.values.collect(pf)))
    }
  }

  "filter" should "filter the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p: Int => Boolean = _ % 2 == 0

    assert(eav.enumerator.mapE(filter(p)).toVector === F.pure(eav.values.filter(p)))
  }

  "filterM" should "filter the stream with a pure effectful function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p:  Int => Boolean    = _ % 2 == 0
    val fp: Int => F[Boolean] = i => F.pure(p(i))

    assert(eav.enumerator.mapE(filterM(fp)).toVector === F.pure(eav.values.filter(p)))
  }

  "sequenceI" should "repeatedly apply an iteratee" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      assert(eav.enumerator.mapE(sequenceI(takeI(n))).toVector === F.pure(eav.values.grouped(n).toVector))
    }
  }

  it should "work with an iteratee that stops early" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      assert(eav.enumerator.mapE(sequenceI(takeI(n))).run(head) === F.pure(eav.values.grouped(n).toVector.headOption))
    }
  }

  "uniq" should "drop duplicate values" in forAll { (xs: Vector[Int]) =>
    val sorted = xs.sorted

    assert(enumVector(sorted).mapE(uniq).toVector === F.pure(sorted.distinct))
  }

  it should "work with an iteratee that stops early" in forAll { (xs: Vector[Int]) =>
    val sorted = xs.sorted

    assert(enumVector(sorted).mapE(uniq).run(head) === F.pure(sorted.distinct.headOption))
  }

  it should "work with known duplicates" in {
    val enumerator = enumVector(Vector(1, 2, 3, 4))
      .append(enumVector(Vector(4, 5, 6, 7)))
      .append(enumOne(7))
      .append(enumOne(8))
      .append(enumVector(Vector(8, 8, 8)))
      .append(enumVector(Vector(9, 10)))
    val result = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(enumerator.mapE(uniq).toVector === F.pure(result))
  }

  "zipWithIndex" should "zip a stream's values with their indices" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = eav.values.zipWithIndex.map {
      case (v, i) => (v, i.toLong)
    }

    assert(eav.enumerator.mapE(zipWithIndex).toVector === F.pure(result))
  }

  it should "work with an iteratee that stops early" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = eav.values.zipWithIndex.map {
      case (v, i) => (v, i.toLong)
    }

    assert(eav.enumerator.mapE(zipWithIndex).run(head) === F.pure(result.headOption))
  }

  "grouped" should "group values from the stream" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      assert(eav.enumerator.mapE(grouped(n)).toVector === F.pure(eav.values.grouped(n).toVector))
    }
  }

  "splitOn" should "split the stream on a predicate" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p: Int => Boolean = _ % 2 == 0

    def splitOnEvens(xs: Vector[Int]): Vector[Vector[Int]] = if (xs.isEmpty) Vector.empty else {
      val (before, after) = xs.span(x => !p(x))

      before +: splitOnEvens(after.drop(1))
    }

    assert(eav.enumerator.mapE(splitOn(p)).toVector === F.pure(splitOnEvens(eav.values)))
  }

  "cross" should "take the cross product of two enumerators" in {
    forAll { (eav1: EnumeratorAndValues[Int], eav2: EnumeratorAndValues[Int]) =>
      val result = for {
        v1 <- eav1.values
        v2 <- eav2.values
      } yield (v1, v2)

      assert(eav1.enumerator.mapE(cross(eav2.enumerator)).toVector === F.pure(result))
    }
  }

  "intersperse" should "intersperse values in the stream with a delimiter" in {
    forAll { (eav: EnumeratorAndValues[Int], delim: Int) =>
      val expected = eav.values.zip(Stream.continually(delim)).flatMap {
        case (x, y) => Vector(x, y)
      }.dropRight(1)

      assert(eav.resultWithLeftovers(consume[Int].through(intersperse(delim))) === F.pure((expected, Vector.empty)))
    }
  }
}
