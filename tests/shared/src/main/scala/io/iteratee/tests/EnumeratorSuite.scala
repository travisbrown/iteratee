package io.iteratee.tests

import algebra.Eq
import algebra.laws.GroupLaws
import cats.Monad
import cats.laws.discipline.{ CartesianTests, MonadTests }
import io.iteratee.Enumerator
import org.scalacheck.{ Gen, Prop }

abstract class EnumeratorSuite[F[_]: Monad] extends ModuleSuite[F] {
  type EnumeratorF[E] = Enumerator[F, E]

  implicit val isomorphisms: CartesianTests.Isomorphisms[EnumeratorF] =
    CartesianTests.Isomorphisms.invariant[EnumeratorF]

  checkAll(s"Enumerator[$monadName, Int]", GroupLaws[Enumerator[F, Int]].monoid)
  checkAll(s"Enumerator[$monadName, Int]", MonadTests[EnumeratorF].monad[Int, Int, Int])

  test("liftM") {
    check { (i: Int) =>
      liftToEnumerator(F.pure(i)).toVector === F.pure(Vector(i))
    }
  }

  test("empty") {
    empty[Int].toVector === F.pure(Vector.empty)
  }

  test("enumOne") {
    check { (i: Int) =>
      enumOne(i).toVector === F.pure(Vector(i))
    }
  }

  test("enumStream") {
    check { (xs: Stream[Int]) =>
      enumStream(xs).toVector === F.pure(xs.toVector)
    }
  }

  test("enumList") {
    check { (xs: List[Int]) =>
      enumList(xs).toVector === F.pure(xs.toVector)
    }
  }

  test("enumVector") {
    check { (xs: Vector[Int]) =>
      enumVector(xs).toVector === F.pure(xs)
    }
  }

  test("enumVector with single element") {
    check { (x: Int) =>
      enumVector(Vector(x)).toVector === F.pure(Vector(x))
    }
  }

  test("enumIndexedSeq") {
    check { (xs: Vector[Int], start: Int, count: Int) =>
      enumIndexedSeq(xs, start, start + count).toVector === F.pure(xs.slice(start, start + count))
    }
  }

  test("enumIndexedSeq with given slice") {
    check { (xs: Vector[Int]) =>
      enumIndexedSeq(xs, 0, 100).toVector === F.pure(xs.slice(0, 100))
    }
  }

  test("repeat") {
    check { (i: Int, count: Short) =>
      repeat(i).run(take(count.toInt)) === F.pure(Vector.fill(count.toInt)(i))
    }
  }

  test("iterate") {
    check { (n: Int, count: Short) =>
      iterate(n)(i => Some(i + 1)).run(take(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1))
    }
  }

  test("pure iterateM") {
    check { (n: Int, count: Short) =>
      val enumerator = iterateM(n){ i => F.pure(if(i == count){
        None
      }else{
        Some(i + 1)
      })}
      enumerator.run(take(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1))
    }
  }

  test("pure generateM") {
    check { (n: Int, count: Short) =>
      var i = n
      val enumerator = generateM(n){ F.pure(if(i == count){
        None
      }else{
        i+=1
        Some(i)
      })}
      enumerator.run(take(count.toInt)) === F.pure(Vector.iterate(n, count.toInt)(_ + 1))
    }
  }

  test("toVector") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.toVector === F.pure(eav.values)
    }
  }

  test("prepend") {
    check { (eav: EnumeratorAndValues[Int], v: Int) =>
      eav.enumerator.prepend(v).toVector === F.pure(v +: eav.values)
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

      E.eqv(enumeratorF.map(_.map(_.toVector)), F.pure(Option(F.pure(eav.values.map(_.toString)))))
    }
  }

  test("intoEnumerator") {
    import syntax._

    check { (i: Int) =>
      F.pure(i).intoEnumerator.toVector === F.pure(Vector(i))
    }
  }

  test("flatten") {
    check { (v: Int) =>
      enumOne(F.pure(v)).flatten[Int].toVector === F.pure(Vector(v))
    }
  }

  test("reduced") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.reduced(Vector.empty[Int])(_ :+ _).toVector === F.pure(Vector(eav.values))
    }
  }

  test("map") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.map(_ + 1).toVector === F.pure(eav.values.map(_ + 1))
    }
  }

  test("mapK") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.enumerator.mapK(i => F.pure(i + 1)).toVector === F.pure(eav.values.map(_ + 1))
    }
  }

  test("flatMap") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val enumerator = eav.enumerator.flatMap(v => enumVector(Vector(v, v)))

      enumerator.toVector === F.pure(eav.values.flatMap(v => Vector(v, v)))
    }
  }
}
