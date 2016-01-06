package io.iteratee

import algebra.Eq
import cats.{ Eval, Id, MonadError }
import cats.arrow.NaturalTransformation
import cats.data.{ Xor, XorT }
import cats.laws.discipline.MonadErrorTests
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeSuite, XorSuite }
import org.scalacheck.{ Arbitrary, Prop }
import org.scalacheck.Prop.BooleanOperators

class XorEnumerateeTests extends EnumerateeSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite

class XorEnumeratorTests extends EnumeratorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite {
  type XTE[A] = XorT[Eval, Throwable, A]

  test("ensure") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = eav.enumerator.ensure(action)

      counter == 0 && enumerator.drain === F.pure(eav.values) && counter === 1
    }
  }

  test("ensure with failure") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = failEnumerator(error).append(eav.enumerator).ensure(action)

      counter == 0 && enumerator.drain.value.value === Xor.left(error) && counter === 1
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

  test("failEnumerator") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)

      eav.enumerator.append(failEnumerator(error)).drain.value.value === Xor.left(error)
    }
  }

  test("handleErrorWith") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val enumerator = failEnumerator(error).handleErrorWith[Throwable](_ => eav.enumerator)

      enumerator.drain.value.value === Xor.right(eav.values)
    }
  }
}

class XorIterateeTests extends IterateeSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite {
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

  test("failIteratee") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val result = MonadError[XTE, Throwable].raiseError[(String, Vector[Int])](error)

      eav.resultWithLeftovers(failIteratee(error)) === result
    }
  }

  test("mapI") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      Prop.forAll(arbitraryIntIteratee[Id].arbitrary) { iteratee =>
        val pureEnumerator = Enumerator.enumVector[Id, Int](eav.values)

        val xorIteratee = iteratee.mapI(
          new NaturalTransformation[Id, XTE] {
            def apply[A](a: A): XTE[A] = F.pure(a)
          }
        )

        eav.enumerator.run(xorIteratee) === F.pure(pureEnumerator.run(iteratee))
      }
    }
  }

  test("up") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      (n != Int.MaxValue) ==> {
        val iteratee = Iteratee.take[Id, Int](n).up[XTE]
        val result = (eav.values.take(n), eav.values.drop(n))

        eav.resultWithLeftovers(iteratee) === F.pure(result)
      }
    }
  }
}