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

      counter === 0 && enumerator.toVector === Eval.now(eav.values) && counter === 2
    }
  }

  test("generateM") {
    check { (n: Short) =>
      val count = math.abs(n.toInt)
      var counter = 0
      val enumerator = generateM(
        Eval.always(
          if (counter > count) None else Some {
            val result = counter
            counter += 1
            result
          }
        )
      )

      enumerator.toVector === Eval.now((0 to count).toVector) && counter == count + 1
    }
  }
}

class EvalIterateeTests extends IterateeSuite[Eval] with EvalSuite
