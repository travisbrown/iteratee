package io.iteratee

import cats.Eval
import io.iteratee.tests.{ EvalSuite, EnumerateeSuite, EnumeratorSuite, IterateeSuite }

class EvalEnumerateeTests extends EnumerateeSuite[Eval] with EvalSuite

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

class EvalIterateeTests extends IterateeSuite[Eval] with EvalSuite
