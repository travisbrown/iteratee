package io.iteratee.testing

import cats.{ Eq, Eval, Monad }
import cats.kernel.laws.GroupLaws
import cats.laws.discipline.{ CartesianTests, MonadTests }
import io.iteratee.{ EnumerateeModule, Enumerator, EnumeratorModule, IterateeModule, Module }
import scala.Predef._

abstract class EnumeratorSuite[F[_]: Monad] extends ModuleSuite[F] {
    this: Module[F] with EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] =>

  type EnumeratorF[E] = Enumerator[F, E]

  implicit val isomorphisms: CartesianTests.Isomorphisms[EnumeratorF] =
    CartesianTests.Isomorphisms.invariant[EnumeratorF]

  checkLaws(s"Enumerator[$monadName, Int]", GroupLaws[Enumerator[F, Int]].monoid)
  checkLaws(s"Enumerator[$monadName, Int]", MonadTests[EnumeratorF].stackUnsafeMonad[Int, Int, Int])

  "liftToEnumerator" should "lift a value in a context into an enumerator" in forAll { (i: Int) =>
    assert(liftToEnumerator(F.pure(i)).toVector === F.pure(Vector(i)))
  }

  "liftMEval" should "lift a value in a context into an enumerator" in forAll { (i: Int) =>
    var counter = 0
    val eval = Eval.later {
      counter += i
      F.pure(i)
    }

    val enumerator = Enumerator.liftMEval(eval)

    assert(counter === 0)
    assert(enumerator.toVector === F.pure(Vector(i)))
    assert(counter === i)
  }

  "enumerate" should "enumerate varargs values" in forAll { (xs: List[Int]) =>
    assert(enumerate(xs: _*).toVector === F.pure(xs.toVector))
  }

  "empty" should "not enumerate any values" in {
    assert(empty[Int].toVector === F.pure(Vector.empty))
  }

  "enumOne" should "enumerate a single value" in forAll { (i: Int) =>
    assert(enumOne(i).toVector === F.pure(Vector(i)))
  }

  "enumIterable" should "enumerate values from an iterable" in forAll { (xs: Iterable[Int], chunkSize: Int) =>
    assert(enumIterable(xs, chunkSize).toVector === F.pure(xs.toVector))
  }

  "enumStream" should "enumerate values from a stream" in forAll { (xs: Stream[Int], chunkSize: Int) =>
    assert(enumStream(xs, chunkSize).toVector === F.pure(xs.toVector))
  }

  "enumList" should "enumerate values from a list" in forAll { (xs: List[Int]) =>
    assert(enumList(xs).toVector === F.pure(xs.toVector))
  }

  "enumVector" should "enumerate values from a vector" in forAll { (xs: Vector[Int]) =>
    assert(enumVector(xs).toVector === F.pure(xs))
  }

  it should "enumerate a vector with a single element" in forAll { (x: Int) =>
    assert(enumVector(Vector(x)).toVector === F.pure(Vector(x)))
  }

  "enumIndexedSeq" should "enumerate a slice of values from an indexed sequence" in {
    forAll { (xs: Vector[Int], start: Int, count: Int) =>
      assert(enumIndexedSeq(xs, start, start + count).toVector === F.pure(xs.slice(start, start + count)))
    }
  }

  it should "enumerate a slice of the first hundred values from an indexed sequence" in {
    forAll { (xs: Vector[Int]) =>
      assert(enumIndexedSeq(xs, 0, 100).toVector === F.pure(xs.slice(0, 100)))
    }
  }

  "repeat" should "repeat a value" in forAll { (i: Int, count: Short) =>
    assert(repeat(i).into(takeI(count.toInt)) === F.pure(Vector.fill(count.toInt)(i)))
  }

  "iterate" should "enumerate values by applying a function iteratively" in forAll { (n: Int, count: Short) =>
    assert(iterate(n)(_ + 1).into(takeI(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1)))
  }

  "iterateM" should "enumerate values by applying a pure function iteratively" in {
    forAll { (n: Int, count: Short) =>
      assert(iterateM(n)(i => F.pure(i + 1)).into(takeI(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1)))
    }
  }

  "iterateUntil" should "apply a function until it returns an empty result" in forAll { (n: Short) =>
    val count = math.abs(n.toInt)
    val enumerator = iterateUntil(0)(i => if (i == count) None else Some(i + 1))

    assert(enumerator.toVector === F.pure((0 to count).toVector))
  }

  it should "work with finished iteratee (#71)" in forAll { (n: Short, fewer: Byte) =>
    val count = math.abs(n.toInt)
    val taken = n - math.abs(fewer.toInt)
    val enumerator = iterateUntil(0)(i => if (i == count) None else Some(i + 1))

    assert(enumerator.into(takeI(taken)) === F.pure((0 to count).toVector.take(taken)))
  }

  "iterateUntilM" should "apply a pure function until it returns an empty result" in forAll { (n: Short) =>
    val count = math.abs(n.toInt)
    val enumerator = iterateUntilM(0)(i => F.pure(if (i == count) None else Some(i + 1)))

    assert(enumerator.toVector === F.pure((0 to count).toVector))
  }

  it should "work with finished iteratee (#71)" in forAll { (n: Short, fewer: Byte) =>
    val count = math.abs(n.toInt)
    val taken = n - math.abs(fewer.toInt)
    val enumerator = iterateUntilM(0)(i => F.pure(if (i == count) None else Some(i + 1)))

    assert(enumerator.into(takeI(taken)) === F.pure((0 to count).toVector.take(taken)))
  }

  "toVector" should "collect all the values in the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.toVector === F.pure(eav.values))
  }

  "prepend" should "prepend a value to a stream" in forAll { (eav: EnumeratorAndValues[Int], v: Int) =>
    assert(eav.enumerator.prepend(v).toVector === F.pure(v +: eav.values))
  }

  it should "work with a done iteratee" in {
    assert(enumOne(0).append(enumOne(2).prepend(1)).into(head) === F.pure((Some(0))))
  }

  "bindM" should "bind through Option" in forAll { (eav: EnumeratorAndValues[Int]) =>
    /**
     * Workaround for divergence during resolution on 2.10.
     */
    val E: Eq[F[Option[F[Vector[String]]]]] = eqF(catsKernelStdEqForOption(eqF(Eq[Vector[String]])))

    val enumeratorF: F[Option[Enumerator[F, String]]] = eav.enumerator.bindM(v => Option(enumOne(v.toString)))

    assert(E.eqv(enumeratorF.map(_.map(_.toVector)), F.pure(Option(F.pure(eav.values.map(_.toString))))))
  }

  "intoEnumerator" should "be available on values in a context" in forAll { (i: Int) =>
    import syntax._

    assert(F.pure(i).intoEnumerator.toVector === F.pure(Vector(i)))
  }

  "flatten" should "collapse enumerated values in the context" in forAll { (v: Int) =>
    assert(enumOne(F.pure(v)).flatten[Int].toVector === F.pure(Vector(v)))
  }

  "reduced" should "reduce the stream with a function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.reduced(Vector.empty[Int])(_ :+ _).toVector === F.pure(Vector(eav.values)))
  }

  it should "reduce the stream with a pure function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.reducedM(Vector.empty[Int])((i, s) => F.pure(i :+ s)).toVector === F.pure(Vector(eav.values)))
  }

  "map" should "transform the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.map(_ + 1).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMapM" should "transform the stream with a pure effectful function" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.flatMapM(i => F.pure(i + 1)).toVector === F.pure(eav.values.map(_ + 1)))
  }

  "flatMap" should "transform the stream with a function into enumerators" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.flatMap(v => enumVector(Vector(v, v)))

      assert(enumerator.toVector === F.pure(eav.values.flatMap(v => Vector(v, v))))
    }
  }

  "take" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int], n: Long) =>
    assert(eav.enumerator.take(n) === eav.enumerator.through(take(n)))
  }

  "takeWhile" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.takeWhile(p) === eav.enumerator.through(takeWhile(p)))
    }
  }

  "takeWhileM" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.takeWhileM(p.andThen(F.pure)) === eav.enumerator.through(takeWhileM(p.andThen(F.pure))))
    }
  }

  "drop" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int], n: Long) =>
    assert(eav.enumerator.drop(n) === eav.enumerator.through(drop(n)))
  }

  "dropWhile" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.dropWhile(p) === eav.enumerator.through(dropWhile(p)))
    }
  }

  "dropWhileM" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.dropWhileM(p.andThen(F.pure)) === eav.enumerator.through(dropWhileM(p.andThen(F.pure))))
    }
  }

  "scan" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], init: String, f: (String, Int) => String) =>
      assert(eav.enumerator.scan(init)(f) === eav.enumerator.through(scan(init)(f)))
    }
  }

  "scanM" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], init: String, f: (String, Int) => String) =>
      val ff: (String, Int) => F[String] = (s, i) => F.pure(f(s, i))

      assert(eav.enumerator.scanM(init)(ff) === eav.enumerator.through(scanM(init)(ff)))
    }
  }

  "collect" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], f: Int => Option[String]) =>
      val pf: PartialFunction[Int, String] = {
        case x if f(x).isDefined => f(x).get
      }
      assert(eav.enumerator.collect(pf) === eav.enumerator.through(collect(pf)))
    }
  }

  "filter" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
    assert(eav.enumerator.filter(p) === eav.enumerator.through(filter(p)))
  }

  "filterM" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.filterM(p.andThen(F.pure)) === eav.enumerator.through(filterM(p.andThen(F.pure))))
    }
  }

  "sequenceI" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.sequenceI(takeI(2)) === eav.enumerator.through(sequenceI(takeI(2))))
  }

  "uniq" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.uniq === eav.enumerator.through(uniq))
  }

  "zipWithIndex" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.zipWithIndex === eav.enumerator.through(zipWithIndex))
  }

  "grouped" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.grouped(2) === eav.enumerator.through(grouped(2)))
  }

  "splitOn" should "match using an Enumeratee directly" in {
    forAll { (eav: EnumeratorAndValues[Int], p: Int => Boolean) =>
      assert(eav.enumerator.splitOn(p) === eav.enumerator.through(splitOn(p)))
    }
  }

  "cross" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.cross(enumList(List(1, 2))) === eav.enumerator.through(cross(enumList(List(1, 2)))))
  }

  "intersperse" should "match using an Enumeratee directly" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.intersperse(-1) === eav.enumerator.through(intersperse(-1)))
  }
}

abstract class StackSafeEnumeratorSuite[F[_]: Monad] extends EnumeratorSuite[F] {
    this: Module[F] with EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] =>

  "StackUnsafe.enumStream" should "be consistent with enumStream" in forAll { (xs: Stream[Int]) =>
    val expected = enumStream(xs).toVector

    assert(Enumerator.StackUnsafe.enumStream[F, Int](xs).toVector === expected)
  }

  "StackUnsafe.repeat" should "be consistent with repeat" in forAll { (i: Int, count: Short) =>
    val expected = repeat(i).into(takeI(count.toInt))

    assert(Enumerator.StackUnsafe.repeat[F, Int](i).into(takeI(count.toInt)) === expected)
  }

  "StackUnsafe.iterate" should "be consistent with iterate" in forAll { (n: Int, count: Short) =>
    val expected = iterate(n)(_ + 1).into(takeI(count.toInt))

    assert(Enumerator.StackUnsafe.iterate[F, Int](n)(_ + 1).into(takeI(count.toInt)) === expected)
  }

  "StackUnsafe.iterateM" should "be consistent with iterateM" in forAll { (n: Int, count: Short) =>
    val expected = iterateM(n)(i => F.pure(i + 1)).into(takeI(count.toInt))

    assert(Enumerator.StackUnsafe.iterateM[F, Int](n)(i => F.pure(i + 1)).into(takeI(count.toInt)) === expected)
  }

  "StackUnsafe.iterateUntil" should "be consistent with iterateUntil" in forAll { (n: Short) =>
    val count = math.abs(n.toInt)
    val expected = iterateUntil(0)(i => if (i == count) None else Some(i + 1)).toVector
    val enumerator = Enumerator.StackUnsafe.iterateUntil[F, Int](0)(i => if (i == count) None else Some(i + 1))

    assert(enumerator.toVector === expected)
  }

  "StackUnsafe.iterateUntilM" should "be consistent with iterateUntilM" in forAll { (n: Short) =>
    val count = math.abs(n.toInt)
    val expected = iterateUntilM(0)(i => F.pure(if (i == count) None else Some(i + 1))).toVector
    val enumerator = Enumerator.StackUnsafe.iterateUntilM[F, Int](0)(i => F.pure(if (i == count) None else Some(i + 1)))

    assert(enumerator.toVector === expected)
  }
}
