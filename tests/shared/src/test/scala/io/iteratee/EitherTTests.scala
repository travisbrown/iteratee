package io.iteratee

import cats.{ Eval, Id, MonadError }
import cats.arrow.FunctionK
import cats.data.EitherT
import cats.laws.discipline.arbitrary.catsLawsCogenForThrowable
import io.iteratee.testing.{ EnumerateeSuite, IterateeErrorSuite, StackSafeEnumeratorSuite }
import io.iteratee.testing.EqInstances.eqThrowable
import io.iteratee.tests.EitherTSuite
import org.scalacheck.Arbitrary

class EitherTEnumerateeTests extends EnumerateeSuite[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
    with EitherTSuite

class EitherTEnumeratorTests extends StackSafeEnumeratorSuite[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
    with EitherTSuite {
  type ETE[A] = EitherT[Eval, Throwable, A]

  "ensure" should "perform an action after the enumerator is done" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var counter = 0
    val action = EitherT.right[Throwable](Eval.always(counter += 1))
    val enumerator = eav.enumerator.ensure(action)

    assert(counter === 0)
    assert(enumerator.toVector === F.pure(eav.values))
    assert(counter === 1)
  }

  it should "perform its action in the case of failure" in forAll { (eav: EnumeratorAndValues[Int], message: String) =>
    val error: Throwable = new Exception(message)
    var counter = 0
    val action = EitherT.right[Throwable](Eval.always(counter += 1))
    val enumerator = failEnumerator(error).append(eav.enumerator).ensure(action)

    assert(counter == 0)
    assert(enumerator.toVector.value.value === Left(error))
    assert(counter === 1)
  }

  it should "work without necessarily consuming all elements" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var counter = 0
    val action = EitherT.right[Throwable](Eval.always(counter += 1))
    val enumerator = eav.enumerator.ensure(action)
    val n = math.max(0, eav.values.size - 2)

    assert(counter == 0)
    assert(enumerator.into(takeI(n)) === F.pure(eav.values.take(n)))
    assert(counter === 1)
  }

  "failEnumerator" should "return a failed enumerator" in forAll { (eav: EnumeratorAndValues[Int], message: String) =>
    val error: Throwable = new Exception(message)

    assert(eav.enumerator.append(failEnumerator(error)).toVector.value.value === Left(error))
  }

  "enumEither" should "either enumerate a single value or fail" in forAll { (either: Either[Throwable, Int]) =>
    assert(enumEither(either).toVector === EitherT.fromEither[Eval](either).map(Vector(_)))
  }

  "handleErrorWith" should "allow recovery from failures" in {
    forAll { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val enumerator = failEnumerator(error).handleErrorWith[Throwable](_ => eav.enumerator)

      assert(enumerator.toVector.value.value === Right(eav.values))
    }
  }
}

class EitherTIterateeTests extends IterateeErrorSuite[({ type L[x] = EitherT[Eval, Throwable, x] })#L, Throwable]
    with EitherTSuite {
  type ETE[A] = EitherT[Eval, Throwable, A]

  "failIteratee" should "return an iteratee that always fails" in {
    forAll { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val result = MonadError[ETE, Throwable].raiseError[(String, Vector[Int])](error)

      assert(eav.resultWithLeftovers(failIteratee(error)) === result)
    }
  }

  "mapI" should "transform the stream with a natural transformation" in {
    forAll(Arbitrary.arbitrary[EnumeratorAndValues[Int]], arbitraryIntIteratee[Id].arbitrary) { (eav, iteratee) =>
      val pureEnumerator = Enumerator.enumVector[Id, Int](eav.values)

      val xorIteratee = iteratee.mapI(
        new FunctionK[Id, ETE] {
          def apply[A](a: A): ETE[A] = F.pure(a)
        }
      )

      assert(eav.enumerator.into(xorIteratee) === F.pure(pureEnumerator.into(iteratee)))
    }
  }

  "up" should "lift an iteratee into a larger context" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    whenever(n != Int.MaxValue) {
      val iteratee = Iteratee.take[Id, Int](n).up[ETE]
      val result = (eav.values.take(n), eav.values.drop(n))

      assert(eav.resultWithLeftovers(iteratee) === F.pure(result))
    }
  }
}
