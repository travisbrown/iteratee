package io.iteratee

import algebra.Eq
import algebra.laws.GroupLaws
import cats.{ Eval, Monad }
import cats.data.XorT
import cats.laws.discipline.{ MonadTests, MonoidalTests }
import org.scalacheck.{ Gen, Prop }

abstract class EnumeratorSuite[F[_]: Monad] extends ModuleSuite[F] {
  type EnumeratorF[E] = Enumerator[F, E]

  implicit val isomorphisms: MonoidalTests.Isomorphisms[EnumeratorF] =
    MonoidalTests.Isomorphisms.invariant[EnumeratorF]

  checkAll(s"Enumerator[$monadName, Int]", GroupLaws[Enumerator[F, Int]].monoid)
  checkAll(s"Enumerator[$monadName, Int]", MonadTests[EnumeratorF].monad[Int, Int, Int])

  test("liftM") {
    check { (i: Int) =>
      liftToEnumerator(F.pure(i)).drain === F.pure(Vector(i))
    }
  }

  test("empty") {
    empty[Int].drain === F.pure(Vector.empty)
  }

  test("enumEnd") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.append(enumEnd[Int]).append(eav.enumerator).drain === F.pure(eav.values)
    }
  }

  test("enumOne") {
    check { (i: Int) =>
      enumOne(i).drain === F.pure(Vector(i))
    }
  }

  test("enumStream") {
    check { (xs: Stream[Int]) =>
      enumStream(xs).drain === F.pure(xs.toVector)
    }
  }

  test("enumList") {
    check { (xs: List[Int]) =>
      enumList(xs).drain === F.pure(xs.toVector)
    }
  }

  test("enumVector") {
    check { (xs: Vector[Int]) =>
      enumVector(xs).drain === F.pure(xs)
    }
  }

  test("enumIndexedSeq") {
    check { (xs: Vector[Int], start: Int, count: Int) =>
      enumIndexedSeq(xs, start, start + count).drain === F.pure(xs.slice(start, start + count))
    }
  }

  test("repeat") {
    check { (i: Int, count: Short) =>
      repeat(i).run(take(count.toInt)) === F.pure(Vector.fill(count.toInt)(i))
    }
  }

  test("iterate") {
    check { (n: Int, count: Short) =>
      iterate(n)(_ + 1).run(take(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1))
    }
  }

  test("pure iterateM") {
    check { (n: Int, count: Short) =>
      val enumerator = iterateM(n)(i => F.pure(i + 1))
      enumerator.run(take(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1))
    }
  }

  test("drain") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.drain === F.pure(eav.values)
    }
  }

  test("drainTo") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.drainTo[List] === F.pure(eav.values.toList)
    }
  }

  test("prepend") {
    check { (eav: EnumeratorAndValues[Int], v: Int) =>
      eav.enumerator.prepend(v).drain === F.pure(v +: eav.values)
    }
  }

  test("prepend with done iteratee") {
    assert(enumOne(0).append(enumOne(2).prepend(1)).run(head) === F.pure((Some(0))))
  }

  test("bindM") {
    check { (eav: EnumeratorAndValues[Int]) =>
      /**
       * Workaround for divergence during resolution on 2.10.
       */
      val E: Eq[F[Option[F[Vector[String]]]]] = eqF(eqOption(eqF(eqVector(stringOrder))))

      val enumeratorF: F[Option[Enumerator[F, String]]] =
        eav.enumerator.bindM(v => Option(enumOne(v.toString)))

      E.eqv(enumeratorF.map(_.map(_.drain)), F.pure(Option(F.pure(eav.values.map(_.toString)))))
    }
  }

  test("intoEnumerator") {
    import syntax._

    check { (i: Int) =>
      F.pure(i).intoEnumerator.drain === F.pure(Vector(i))
    }
  }

  test("flatten") {
    check { (v: Int) =>
      enumOne(F.pure(v)).flatten[Int].drain === F.pure(Vector(v))
    }
  }

  test("reduced") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.reduced(Vector.empty[Int])(_ :+ _).drain === F.pure(Vector(eav.values))
    }
  }

  test("map") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.map(_ + 1).drain === F.pure(eav.values.map(_ + 1))
    }
  }

  test("flatMap") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.flatMap(v => enumVector(Vector(v, v)))

      enumerator.drain === F.pure(eav.values.flatMap(v => Vector(v, v)))
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

      eav.enumerator.collect(pf).drain === F.pure(eav.values.collect(pf))
    }
  }

  test("filter") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val p: Int => Boolean = _ % 2 == 0

      eav.enumerator.filter(p).drain === F.pure(eav.values.filter(p))
    }
  }

  test("sequenceI") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.sequenceI(take(n)).drain === F.pure(eav.values.grouped(n).toVector)
      }
    }
  }

  test("uniq") {
    check { (xs: Vector[Int]) =>
      val sorted = xs.sorted

      enumVector(sorted).uniq.drain === F.pure(sorted.distinct)
    }
  }

  test("zipWithIndex") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = eav.values.zipWithIndex.map {
        case (v, i) => (v, i.toLong)
      }

      eav.enumerator.zipWithIndex.drain === F.pure(result)
    }
  }

  test("grouped") {
    check { (eav: EnumeratorAndValues[Int]) =>
      Prop.forAll(Gen.posNum[Int]) { n =>
        eav.enumerator.grouped(n).drain === F.pure(eav.values.grouped(n).toVector)
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

      eav.enumerator.splitOn(p).drain === F.pure(splitOnEvens(eav.values))
    }
  }

  test("cross") {
    check { (eav1: EnumeratorAndValues[Int], eav2: EnumeratorAndValues[Int]) =>
      val result = for {
        v1 <- eav1.values
        v2 <- eav2.values
      } yield (v1, v2)

      eav1.enumerator.cross(eav2.enumerator).drain === F.pure(result)
    }
  }
}

class EvalEnumeratorTests extends EnumeratorSuite[Eval] with EvalSuite {
  test("perform") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var counter = 0
      val action = perform[Int](Eval.always(counter += 1))
      val enumerator = action.append(eav.enumerator).append(action)

      counter === 0 && enumerator.drain === F.pure(eav.values) && counter === 2
    }
  }
}

class XorEnumeratorTests extends EnumeratorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L]
  with XorSuite {
  test("ensure") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = eav.enumerator.ensure(action)

      counter == 0 && enumerator.drain === F.pure(eav.values) && counter === 1
    }
  }

  test("ensure without necessarily consuming all elements") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = eav.enumerator.ensure(action)
      val n = math.max(0, eav.values.size - 2)

      counter == 0 && enumerator.run(take(n)) === F.pure(eav.values.take(n)) && counter === 1
    }
  }
}
