package io.iteratee.testing

import cats.Monad
import cats.laws.discipline.{ CategoryTests, ProfunctorTests }
import io.iteratee.{ Enumeratee, EnumerateeModule, EnumeratorModule, Iteratee, IterateeModule, Module }
import org.scalacheck.{ Arbitrary, Gen }
import scala.Predef._

abstract class EnumerateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  this: Module[F] with EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] =>

  type EnumerateeF[O, I] = Enumeratee[F, O, I]

  checkLaws(s"Enumeratee[$monadName, Int, Int]", ProfunctorTests[EnumerateeF].profunctor[Int, Int, Int, Int, Int, Int])
  checkLaws(s"Enumeratee[$monadName, Int, Int]", CategoryTests[EnumerateeF].category[Int, Int, Int, Int])

  "into" should "transform the inputs to an iteratee" in forAll {
    (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int], enumeratee: Enumeratee[F, Int, Int]) =>
      eav.enumerator.into(enumeratee.into(iteratee)) === eav.enumerator.into(iteratee.through(enumeratee))
  }

  "map" should "transform the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.through(map(_ + 1)).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMapM" should "transform the stream with a pure effectful function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.through(flatMapM(i => F.pure(i + 1))).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMap" should "transform the stream with a function into enumerators" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.through(flatMap(v => enumVector(Vector(v, v))))

      assert(enumerator.toVector === F.pure(eav.values.flatMap(v => Vector(v, v))))
    }
  }

  it should "work with an iteratee that stops early" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val enumerator = eav.enumerator.through(flatMap(v => enumVector(Vector(v, v))))

    assert(enumerator.into(head) === F.pure(eav.values.flatMap(v => Vector(v, v)).headOption))
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

  it should "work with more than Int.MaxValue values" in forAll { (n: Int) =>
    val items = Vector.fill(1000000)(())
    val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
    val enumerator = repeat(()).flatMap(_ => enumVector(items)).through(take(totalSize))

    assert(enumerator.into(length) === F.pure(totalSize))
  }

  it should "work when it ends mid-chunk" in forAll { (v: Vector[Int]) =>
    assert(enumVector(v).through(take(v.size.toLong - 1L)).toVector === F.pure(v.dropRight(1)))
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

    assert(enumVector(v).through(takeWhile(_ < n.toInt)).toVector === F.pure(v.dropRight(1)))
  }

  "takeWhileM" should "consume the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val result = eav.resultWithLeftovers(consume[Int].through(takeWhileM(i => F.pure(i < n))))
    val expected = F.pure(eav.values.span(_ < n))
    assert(result === expected)
  }

  it should "work with wrap" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val eavNew = eav.copy(enumerator = takeWhileM[Int](i => F.pure(i < n)).wrap(eav.enumerator))

    assert(eavNew.resultWithLeftovers(consume) === F.pure((eav.values.takeWhile(_ < n), Vector.empty)))
  }

  it should "work when it ends mid-chunk" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).through(takeWhileM(i => F.pure(i < n.toInt))).toVector === F.pure(v.dropRight(1)))
  }

  "drop" should "drop the specified number of values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    assert(eav.resultWithLeftovers(consume[Int].through(drop(n.toLong))) === F.pure((eav.values.drop(n), Vector.empty)))
  }

  it should "work with one left over" in forAll { (v: Vector[Int]) =>
    assert(enumVector(v).through(drop(v.size.toLong - 1L)).toVector === F.pure(v.lastOption.toVector))
  }

  it should "work with more than Int.MaxValue values" in forAll { (n: Int) =>
    val items = Vector.fill(1000000)(())
    val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
    val enumerator = repeat(()).flatMap(_ => enumVector(items)).through(drop(totalSize))

    assert(enumerator.into(head) === F.pure(Some(())))
  }

  "dropWhile" should "drop the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val expected = F.pure((eav.values.dropWhile(_ < n), Vector.empty[Int]))

    assert(eav.resultWithLeftovers(consume[Int].through(dropWhile(_ < n))) === expected)
  }

  it should "work with one left over" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).through(dropWhile(_ < n.toInt)).toVector === F.pure(v.lastOption.toVector))
  }

  "dropWhileM" should "drop the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    val expected = F.pure((eav.values.dropWhile(_ < n), Vector.empty[Int]))

    assert(eav.resultWithLeftovers(consume[Int].through(dropWhileM(i => F.pure(i < n)))) === expected)
  }

  it should "work with one left over" in forAll { (n: Byte) =>
    val v = (0 to n.toInt).toVector

    assert(enumVector(v).through(dropWhileM(i => F.pure(i < n.toInt))).toVector === F.pure(v.lastOption.toVector))
  }

  "collect" should "filter the stream using a partial function" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val pf: PartialFunction[Int, Int] = {
        case v if v % 2 == 0 => v + 1
      }

      assert(eav.enumerator.through(collect(pf)).toVector === F.pure(eav.values.collect(pf)))
    }
  }

  "filter" should "filter the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p: Int => Boolean = _ % 2 == 0

    assert(eav.enumerator.through(filter(p)).toVector === F.pure(eav.values.filter(p)))
  }

  "filterM" should "filter the stream with a pure effectful function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p:  Int => Boolean    = _ % 2 == 0
    val fp: Int => F[Boolean] = i => F.pure(p(i))

    assert(eav.enumerator.through(filterM(fp)).toVector === F.pure(eav.values.filter(p)))
  }

  "sequenceI" should "repeatedly apply an iteratee" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      assert(eav.enumerator.through(sequenceI(takeI(n))).toVector === F.pure(eav.values.grouped(n).toVector))
    }
  }

  it should "work with an iteratee that stops early" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      val expected = eav.values.grouped(n).toVector.headOption

      assert(eav.enumerator.through(sequenceI(takeI(n))).into(head) === F.pure(expected))
    }
  }

  it should "be stack safe even for large chunks" in {
    val groupedSize = 3
    val xs = (0 until 10000).toVector
    val expected = xs.grouped(groupedSize).size.toLong

    assert(enumVector(xs).sequenceI(takeI(groupedSize)).into(length) === F.pure(expected))
  }

  "scan" should "match the standard library's scanLeft" in {
    forAll { (eav: EnumeratorAndValues[Int], init: String, f: (String, Int) => String) =>
      assert(eav.enumerator.through(scan(init)(f)).toVector === F.pure(eav.values.scanLeft(init)(f)))
    }
  }

  "scanM" should "match the standard library's scanLeft with pure" in {
    forAll { (eav: EnumeratorAndValues[Int], init: String, f: (String, Int) => String) =>
      val ff: (String, Int) => F[String] = (s, i) => F.pure(f(s, i))

      assert(eav.enumerator.scanM(init)(ff).toVector === F.pure(eav.values.scanLeft(init)(f)))
    }
  }

  "remainderWithResult" should "return an empty result for iteratees that consume all input" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val enumeratee = remainderWithResult(consume[Int])((r, i) => r)

      assert(eav.enumerator.through(enumeratee).toVector === F.pure(Vector.empty))
    }
  }

  it should "add the first n values to subsequent values" in {
    forAll { (eav: EnumeratorAndValues[Int], n: Byte) =>
      val enumeratee = remainderWithResult(takeI[Int](n.toInt))((r, i) => i + r.sum)

      val (firstN, rest) = eav.values.splitAt(n.toInt)
      val expected = rest.map(_ + firstN.sum)

      assert(eav.enumerator.through(enumeratee).toVector === F.pure(expected))
    }
  }

  "remainderWithResultM" should "return an empty result for iteratees that consume all input" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val enumeratee = remainderWithResultM(consume[Int])((r, i) => F.pure(r))

      assert(eav.enumerator.through(enumeratee).toVector === F.pure(Vector.empty))
    }
  }

  it should "add the first n values to subsequent values" in {
    forAll { (eav: EnumeratorAndValues[Int], n: Byte) =>
      val enumeratee = remainderWithResultM(takeI[Int](n.toInt))((r, i) => F.pure(i + r.sum))

      val (firstN, rest) = eav.values.splitAt(n.toInt)
      val expected = rest.map(_ + firstN.sum)

      assert(eav.enumerator.through(enumeratee).toVector === F.pure(expected))
    }
  }

  "uniq" should "drop duplicate values" in forAll { (xs: Vector[Int]) =>
    val sorted = xs.sorted

    assert(enumVector(sorted).through(uniq).toVector === F.pure(sorted.distinct))
  }

  it should "work with an iteratee that stops early" in forAll { (xs: Vector[Int]) =>
    val sorted = xs.sorted

    assert(enumVector(sorted).through(uniq).into(head) === F.pure(sorted.distinct.headOption))
  }

  it should "work with known duplicates" in {
    val enumerator = enumVector(Vector(1, 2, 3, 4))
      .append(enumVector(Vector(4, 5, 6, 7)))
      .append(enumOne(7))
      .append(enumOne(8))
      .append(enumVector(Vector(8, 8, 8)))
      .append(enumVector(Vector(9, 10)))
    val result = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(enumerator.through(uniq).toVector === F.pure(result))
  }

  "zipWithIndex" should "zip a stream's values with their indices" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = eav.values.zipWithIndex.map {
      case (v, i) => (v, i.toLong)
    }

    assert(eav.enumerator.through(zipWithIndex).toVector === F.pure(result))
  }

  it should "work with an iteratee that stops early" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = eav.values.zipWithIndex.map {
      case (v, i) => (v, i.toLong)
    }

    assert(eav.enumerator.through(zipWithIndex).into(head) === F.pure(result.headOption))
  }

  "grouped" should "group values from the stream" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], Gen.posNum[Int]) { (eav, n) =>
      assert(eav.enumerator.through(grouped(n)).toVector === F.pure(eav.values.grouped(n).toVector))
    }
  }

  "splitOn" should "split the stream on a predicate" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val p: Int => Boolean = _ % 2 == 0

    def splitOnEvens(xs: Vector[Int]): Vector[Vector[Int]] = if (xs.isEmpty) Vector.empty else {
      val (before, after) = xs.span(x => !p(x))

      before +: splitOnEvens(after.drop(1))
    }

    assert(eav.enumerator.through(splitOn(p)).toVector === F.pure(splitOnEvens(eav.values)))
  }

  "cross" should "take the cross product of two enumerators" in {
    forAll { (eav1: EnumeratorAndValues[Int], eav2: EnumeratorAndValues[Int]) =>
      val result = for {
        v1 <- eav1.values
        v2 <- eav2.values
      } yield (v1, v2)

      assert(eav1.enumerator.through(cross(eav2.enumerator)).toVector === F.pure(result))
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

  "injectValue" should "add a value at the head of the stream" in {
    forAll { (eav: EnumeratorAndValues[Int], e: Int) =>
      val expected = e +: eav.values

      assert(eav.resultWithLeftovers(consume[Int].through(injectValue(e))) === F.pure((expected, Vector.empty)))
    }
  }

  "injectValues" should "add values at the head of the stream" in {
    forAll { (eav: EnumeratorAndValues[Int], es: Seq[Int]) =>
      val expected = es.toVector ++ eav.values

      assert(eav.resultWithLeftovers(consume[Int].through(injectValues(es))) === F.pure((expected, Vector.empty)))
    }
  }

  "chunks" should "observe chunks" in forAll { (vs: Vector[Vector[Int]]) =>
    val cs = vs.filter(_.nonEmpty)

    val enumerator = cs.foldLeft(empty[Int]) {
      case (e, chunk) => e.append(enumVector(chunk))
    }

    assert(enumerator.through(Enumeratee.chunks[F, Int]).toVector === F.pure(cs))
  }

  "rechunk" should "work correctly" in forAll { (eav: EnumeratorAndValues[Int], n: Byte) =>
    val expected = eav.values.grouped(if (n > 0) n.toInt else 1).toVector
    val enumeratee = Enumeratee.rechunk[F, Int](n.toInt).andThen(Enumeratee.chunks)

    assert(eav.enumerator.through(enumeratee).toVector === F.pure(expected))
  }

  it should "correctly handle some corner cases" in {
    val enumerator1 = enumIndexedSeq(0 to 20).through(Enumeratee.rechunk(5))
    val enumerator2 = iterate(0)(_ + 1).through(Enumeratee.rechunk(5))
    val enumerator3 = enumVector((0 until 5).toVector)
    val enumerator4 = enumerator3.append(enumerator3).through(Enumeratee.rechunk(5))

    assert(enumerator1.into(takeI(6)) === F.pure((0 until 6).toVector))
    assert(enumerator2.into(takeI(6)) === F.pure((0 until 6).toVector))
    assert(enumerator4.toVector === F.pure(((0 until 5) ++ (0 until 5)).toVector))
  }
}
