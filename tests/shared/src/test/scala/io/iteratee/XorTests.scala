package io.iteratee

import cats.{ Eval, Id, MonadError }
import cats.arrow.FunctionK
import cats.data.{ Xor, XorT }
import io.iteratee.tests.{ EnumerateeSuite, EnumeratorSuite, IterateeErrorSuite, XorSuite, eqThrowable }
import org.scalacheck.Arbitrary

class XorEnumerateeTests extends EnumerateeSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite {
  "take" should "work with more than Int.MaxValue values" in forAll { (n: Int) =>
    val items = Vector.fill(1000000)(())
    val totalSize: Long = Int.MaxValue.toLong + math.max(1, n).toLong
    val enumerator = repeat(()).flatMap(_ => enumVector(items)).mapE(take(totalSize))

    assert(enumerator.run(length) === F.pure(totalSize))
  }
}

class XorEnumeratorTests extends EnumeratorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorSuite {
  type XTE[A] = XorT[Eval, Throwable, A]

  "ensure" should "perform an action after the enumerator is done" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var counter = 0
    val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
    val enumerator = eav.enumerator.ensure(action)

    assert(counter === 0)
    assert(enumerator.toVector === F.pure(eav.values))
    assert(counter === 1)
  }

  it should "perform its action in the case of failure" in forAll { (eav: EnumeratorAndValues[Int], message: String) =>
    val error: Throwable = new Exception(message)
    var counter = 0
    val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
    val enumerator = failEnumerator(error).append(eav.enumerator).ensure(action)

    assert(counter == 0)
    assert(enumerator.toVector.value.value === Xor.left(error))
    assert(counter === 1)
  }

  it should "work without necessarily consuming all elements" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var counter = 0
    val action = XorT.right[Eval, Throwable, Unit](Eval.always(counter += 1))
    val enumerator = eav.enumerator.ensure(action)
    val n = math.max(0, eav.values.size - 2)

    assert(counter == 0)
    assert(enumerator.run(takeI(n)) === F.pure(eav.values.take(n)))
    assert(counter === 1)
  }

  "failEnumerator" should "return a failed enumerator" in forAll { (eav: EnumeratorAndValues[Int], message: String) =>
    val error: Throwable = new Exception(message)

    assert(eav.enumerator.append(failEnumerator(error)).toVector.value.value === Xor.left(error))
  }

  "handleErrorWith" should "allow recovery from failures" in {
    forAll { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val enumerator = failEnumerator(error).handleErrorWith[Throwable](_ => eav.enumerator)

      assert(enumerator.toVector.value.value === Xor.right(eav.values))
    }
  }
}

class XorIterateeTests extends IterateeErrorSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L, Throwable]
  with XorSuite {
  type XTE[A] = XorT[Eval, Throwable, A]

  "failIteratee" should "return an iteratee that always fails" in {
    forAll { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val result = MonadError[XTE, Throwable].raiseError[(String, Vector[Int])](error)

      assert(eav.resultWithLeftovers(failIteratee(error)) === result)
    }
  }

  "mapI" should "transform the stream with a natural transformation" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], arbitraryIntIteratee[Id].arbitrary) { (eav, iteratee) =>
      val pureEnumerator = Enumerator.enumVector[Id, Int](eav.values)

      val xorIteratee = iteratee.mapI(
        new FunctionK[Id, XTE] {
          def apply[A](a: A): XTE[A] = F.pure(a)
        }
      )

      assert(eav.enumerator.run(xorIteratee) === F.pure(pureEnumerator.run(iteratee)))
    }
  }

  "up" should "lift an iteratee into a larger context" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    whenever(n != Int.MaxValue) {
      val iteratee = Iteratee.take[Id, Int](n).up[XTE]
      val result = (eav.values.take(n), eav.values.drop(n))

      assert(eav.resultWithLeftovers(iteratee) === F.pure(result))
    }
  }
}
