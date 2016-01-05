package io.iteratee

import algebra.Eq
import cats.{ Eval, Id, Monad, MonadError }
import cats.arrow.NaturalTransformation
import cats.data.{ NonEmptyVector, Xor, XorT }
import cats.laws.discipline.{ ContravariantTests, MonadErrorTests, MonadTests, MonoidalTests }
import io.iteratee.internal.Step
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

  test("cont") {
    check { (eav: EnumeratorAndValues[Int]) =>
      def myDrain(acc: List[Int]): Iteratee[F, Int, List[Int]] = cont[Int, List[Int]](
        els => myDrain(acc ::: (els.head :: els.tail.toList)),
        F.pure(acc)
      )

      eav.enumerator.run(myDrain(Nil)) === F.map(eav.enumerator.drain)(_.toList)
    }
  }

  test("done with no leftovers") {
    check { (eav: EnumeratorAndValues[Int], s: String) =>
      eav.resultWithLeftovers(done(s)) === F.pure((s, eav.values))
    }
  }

  test("done with leftovers") {
    check { (eav: EnumeratorAndValues[Int], s: String, es: Vector[Int]) =>
      eav.resultWithLeftovers(done(s, es)) === F.pure((s, es ++ eav.values))
    }
  }

  test("ended") {
    check { (eav: EnumeratorAndValues[Int], s: String) =>
      eav.resultWithLeftovers(ended(s)) === F.pure((s, Vector.empty))
    }
  }

  test("liftM") {
    check { (i: Int) =>
      liftToIteratee(F.pure(i)).run === F.pure(i)
    }
  }

  test("identity") {
    check { (eav: EnumeratorAndValues[Int], it: Iteratee[F, Int, Int]) =>
      eav.resultWithLeftovers(identity) === F.pure(((), eav.values)) &&
      eav.resultWithLeftovers(identity.flatMap(_ => it)) === eav.resultWithLeftovers(it)
    }
  }

  test("drain") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = eav.resultWithLeftovers(drain)
      result === F.pure((eav.values, Vector.empty)) &&
      result === eav.resultWithLeftovers(identity.flatMap(_ => drain))
    }
  }

  test("drainTo") {
    check { (eav: EnumeratorAndValues[Int]) =>
      eav.resultWithLeftovers(drainTo[Int, List]) === F.pure((eav.values.toList, Vector.empty))
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

  test("dropWhile with nothing left in chunk") {
    val iteratee = for {
      _ <- dropWhile[Int](_ < 100)
      r <- drain
    } yield r
    enumVector(Vector(1, 2, 3)).run(iteratee) === F.pure(Vector(1, 2, 3))
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
      eav.resultWithLeftovers(drain.flatMap(_ => isEnd)) === F.pure((true, Vector.empty))
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

  test("zip where leftover sizes must be compared") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val iteratee = take[Int](2).zip(take(3))

      val result = ((eav.values.take(2), eav.values.take(3)), eav.values.drop(3))

      eav.resultWithLeftovers(iteratee) === F.pure(result)
    }
  }

  test("zip on ended iteratees") {
    check { (eav: EnumeratorAndValues[Int], s: String, t: String) =>
      val iteratee = ended[Int, String](s).zip(ended(t))

      eav.resultWithLeftovers(iteratee) === F.pure(((s, t), Vector.empty))
    }
  }

  test("zip on ended iteratee with cont") {
    check { (eav: EnumeratorAndValues[Int], s: String) =>
      val iteratee = ended[Int, String](s).zip(drain)

      eav.resultWithLeftovers(iteratee) === F.pure(((s, Vector.empty), Vector.empty))
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

  test("foldMap") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = F.pure((eav.values.sum + eav.values.size, Vector.empty[Int]))

      eav.resultWithLeftovers(foldMap(_ + 1)) === result
    }
  }

  test("intoIteratee") {
    import syntax._

    check { (i: Int) =>
      F.pure(i).intoIteratee.run === F.pure(i)
    }
  }

  test("folding done with no leftovers") {
    check { (s: String) =>
      done[Int, String](s).fold(_ => None, (v, r) => Some((v, r)), _ => None) === F.pure(Some((s, Vector.empty)))
    }
  }

  test("folding done with leftovers") {
    check { (s: String, es: Vector[Int]) =>
      done[Int, String](s, es).fold(_ => None, (v, r) => Some((v, r)), _ => None) === F.pure(Some((s, es)))
    }
  }

  test("folding ended") {
    check { (s: String) =>
      ended[Int, String](s).fold(_ => None, (_, _) => None, v => Some(v)) === F.pure(Some((s)))
    }
  }

  test("folding cont") {
    def myDrain(acc: List[Int]): Iteratee[F, Int, List[Int]] = cont[Int, List[Int]](
      els => myDrain(acc ::: (els.head :: els.tail.toList)),
      F.pure(acc)
    )

    check { (es: List[Int]) =>
      val folded = myDrain(es).fold[F[List[Int]]](
        _(NonEmptyVector(0)).run,
        (_, _) => F.pure(Nil),
        _ => F.pure(Nil)
      )

      F.flatten(folded) === F.pure(es :+ 0)
    }
  }
}

class EvalIterateeTests extends IterateeSuite[Eval] with EvalSuite

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

  test("failIteratee") {
    check { (eav: EnumeratorAndValues[Int], message: String) =>
      val error: Throwable = new Exception(message)
      val result = MonadError[XTE, Throwable].raiseError[(String, Vector[Int])](error)

      eav.resultWithLeftovers(failIteratee(error)) === result
    }
  }

  test("mapI") {
    check { (eav: EnumeratorAndValues[Int], n: Int) =>
      (n != Int.MaxValue) ==> {
        val iteratee = Iteratee.take[Id, Int](n).mapI(
          new NaturalTransformation[Id, XTE] {
            def apply[A](a: A): XTE[A] = F.pure(a)
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
        val iteratee = Iteratee.take[Id, Int](n).up[XTE]
        val result = (eav.values.take(n), eav.values.drop(n))

        eav.resultWithLeftovers(iteratee) === F.pure(result)
      }
    }
  }
}
