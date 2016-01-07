package io.iteratee

import algebra.Eq
import cats.{ Eval, Id, MonadError }
import cats.arrow.NaturalTransformation
import cats.data.{ Xor, XorT }
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, XorSuite, eqThrowable }
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators

class XorEnumerateeTests extends EnumerateeSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite

class XorEnumeratorTests extends EnumeratorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite {
  type XTE[A] = XorT[Eval, Throwable, A]

  test("ensure") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = eav.enumerator.ensure(action)

      counter == 0 && enumerator.toVector === F.pure(eav.values) && counter === 1
    }
  }

  test("ensure with failure") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      var counter = 0
      val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
      val enumerator = failEnumerator(error).append(eav.enumerator).ensure(action)

      counter == 0 && enumerator.toVector.value.value === Xor.left(error) && counter === 1
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

      eav.enumerator.append(failEnumerator(error)).toVector.value.value === Xor.left(error)
    }
  }

  test("handleErrorWith") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val enumerator = failEnumerator(error).handleErrorWith[Throwable](_ => eav.enumerator)

      enumerator.toVector.value.value === Xor.right(eav.values)
    }
  }
}

class XorIterateeTests extends IterateeErrorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable]
  with XorSuite {
  type XTE[A] = XorT[Eval, Throwable, A]

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
