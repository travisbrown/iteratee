package io.iteratee

import algebra.Eq
import cats.{ Eval, Id, Monad, MonadError }
import cats.arrow.NaturalTransformation
import cats.data.{ Xor, XorT }
import cats.laws.discipline.{ ContravariantTests, MonadErrorTests, MonadTests, MonoidalTests }
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.BooleanOperators

abstract class IterateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(
    minSize = 0,
    maxSize = 10000
  )

  type VectorIntProducingIteratee[E] = Iteratee[F, E, Vector[Int]]
  type VectorIntFoldingIteratee[A] = Iteratee[F, Vector[Int], A]

  implicit val isomorphisms: MonoidalTests.Isomorphisms[VectorIntFoldingIteratee] =
    MonoidalTests.Isomorphisms.invariant[VectorIntFoldingIteratee]

  checkAll(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadTests[VectorIntFoldingIteratee].monad[Vector[Int], Vector[Int], Vector[Int]]
  )

  checkAll(
    s"Iteratee[$monadName, Int, Vector[Int]]",
    ContravariantTests[VectorIntProducingIteratee].contravariant[Vector[Int], Int, Vector[Int]]
  )

  checkAll(
    s"Iteratee[$monadName, Int, Vector[Int]]",
    ContravariantTests[VectorIntProducingIteratee].invariant[Vector[Int], Int, Vector[Int]]
  )

  test("liftM") {
    check { (i: Int) =>
      liftToIteratee(F.pure(i)).run === F.pure(i)
    }
  }

  test("identity") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(identity) === F.pure(((), eav.values))
    }
  }

  test("consume") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(consume) === F.pure((eav.values, Vector.empty))
    }
  }

  test("consumeIn") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(consumeIn[Int, List]) === F.pure((eav.values.toList, Vector.empty))
    }
  }

  test("reversed") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(reversed) === F.pure((eav.values.toList.reverse, Vector.empty))
    }
  }

  test("head") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = (eav.values.headOption, eav.values.drop(1))

      eav.resultWithLeftovers(head[Int]) === F.pure(result)
    }
  }

  test("peek") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = (eav.values.headOption, eav.values)

      eav.resultWithLeftovers(peek[Int]) === F.pure(result)
    }
  }

  test("take") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      (n != Int.MaxValue) ==> {
        eav.resultWithLeftovers(take[Int](n)) === F.pure((eav.values.take(n), eav.values.drop(n)))
      }
    }
  }

  test("takeWhile") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      eav.resultWithLeftovers(takeWhile(_ < n)) === F.pure(eav.values.span(_ < n))
    }
  }

  test("drop") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      (n != Int.MaxValue) ==> {
        eav.resultWithLeftovers(drop[Int](n)) === F.pure(((), eav.values.drop(n)))
      }
    }
  }

  test("dropWhile") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      eav.resultWithLeftovers(dropWhile(_ < n)) === F.pure(((), eav.values.dropWhile(_ < n)))
    }
  }

  test("fold") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(fold[Int, Int](0)(_ + _)) === F.pure((eav.values.sum, Vector.empty))
    }
  }

  test("foldM") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = (eav.values.sum, Vector.empty)

      eav.resultWithLeftovers(foldM[Int, Int](0)((acc, i) => F.pure(acc + i))) === F.pure(result)
    }
  }

  test("length") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(length) === F.pure((eav.values.size, Vector.empty))
    }
  }

  test("sum") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(sum) === F.pure((eav.values.sum, Vector.empty))
    }
  }

  test("isEnd") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(isEnd) === F.pure((eav.values.isEmpty, eav.values)) &&
      eav.resultWithLeftovers(consume.flatMap(_ => isEnd)) === F.pure((true, Vector.empty))
    }
  }

  test("contramap") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = (eav.values.sum + eav.values.size, Vector.empty)

      eav.resultWithLeftovers(sum[Int].contramap((_: Int) + 1)) === F.pure(result)
    }
  }

  test("through") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = (eav.values.sum + eav.values.size, Vector.empty)

      eav.resultWithLeftovers(sum[Int].through(map(_ + 1))) === F.pure(result)
    }
  }

  test("zip") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = ((eav.values.sum, eav.values.size), Vector.empty)

      eav.resultWithLeftovers(sum[Int].zip(length)) === F.pure(result)
    }
  }

  test("zip with leftovers (scalaz/scalaz#1068)") {
    check { (eav: EnumeratorAndValues[Int], m: Int, n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      (m != Int.MaxValue && n != Int.MaxValue) ==> {
        val result = ((eav.values.take(m), eav.values.take(n)), eav.values.drop(math.max(m, n)))

        eav.resultWithLeftovers(take[Int](m).zip(take[Int](n))) === F.pure(result)
      }
    }
  }

  test("intoIteratee") {
    import syntax._

    check { (i: Int) =>
      F.pure(i).intoIteratee.run === F.pure(i)
    }
  }
}

class EvalIterateeTests extends IterateeSuite[Eval] with EvalSuite {
  test("mapI") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      (n != Int.MaxValue) ==> {
        val iteratee = Iteratee.take[Id, Int](n).mapI(
          new NaturalTransformation[Id, Eval] {
            def apply[A](a: A): Eval[A] = F.pure(a)
          }
        )

        val result = (eav.values.take(n), eav.values.drop(n))

        eav.resultWithLeftovers(iteratee) === F.pure(result)
      }
    }
  }


  test("up") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      (n != Int.MaxValue) ==> {
        val iteratee = Iteratee.take[Id, Int](n).up[Eval]
        val result = (eav.values.take(n), eav.values.drop(n))

        eav.resultWithLeftovers(iteratee) === F.pure(result)
      }
    }
  }
}

class XorIterateeTests extends IterateeSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L]
  with XorSuite {

  type XTE[A] = XorT[Eval, Throwable, A]

  implicit val monadError: MonadError[VectorIntFoldingIteratee, Throwable] =
    Iteratee.iterateeMonadError[
    ({ type L[x] = XorT[Eval, Throwable, x] })#L,
    Throwable,
    Vector[Int]
  ]

  implicit val arbitraryVectorIntFoldingIteratee: Arbitrary[
    VectorIntFoldingIteratee[Vector[Int]]
  ] = arbitraryVectorIteratee[({ type L[x] = XorT[Eval, Throwable, x] })#L, Int]

  implicit val eqVectorIntIteratee: Eq[
    VectorIntFoldingIteratee[Vector[Int]]
  ] = eqIteratee[XTE, Vector[Int], Vector[Int]]

  implicit val eqXorUnitIteratee: Eq[
    VectorIntFoldingIteratee[Xor[Throwable, Unit]]
  ] = eqIteratee[XTE, Vector[Int], Xor[Throwable, Unit]]

  implicit val eqXorVectorIntIteratee: Eq[
    VectorIntFoldingIteratee[Xor[Throwable, Vector[Int]]]
  ] = eqIteratee[XTE, Vector[Int], Xor[Throwable, Vector[Int]]]

  implicit val eqXorTVectorInt3Iteratee: Eq[
    VectorIntFoldingIteratee[(Vector[Int], Vector[Int], Vector[Int])]
  ] = eqIteratee[XTE, Vector[Int], (Vector[Int], Vector[Int], Vector[Int])]

  implicit val eqXorTVectorInt: Eq[
    XorT[({ type L[x] = Iteratee[XTE, Vector[Int], x] })#L, Throwable, Vector[Int]]
  ] = XorT.xorTEq(eqXorVectorIntIteratee)

  implicit val arbitraryVectorIntFunctionIteratee: Arbitrary[
    VectorIntFoldingIteratee[Vector[Int] => Vector[Int]]
  ] = arbitraryFunctionIteratee[XTE, Vector[Int]]

  checkAll(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadErrorTests[VectorIntFoldingIteratee, Throwable].monadError[
      Vector[Int],
      Vector[Int],
      Vector[Int]
    ]
  )
}
