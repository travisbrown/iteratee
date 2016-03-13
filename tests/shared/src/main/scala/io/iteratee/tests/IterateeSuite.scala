package io.iteratee.tests

import algebra.Eq
import cats.{ Monad, MonadError }
import cats.data.{ NonEmptyVector, Xor, XorT }
import cats.laws.discipline.{ CartesianTests, ContravariantTests, MonadTests, MonadErrorTests }
import io.iteratee.Iteratee
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.BooleanOperators

abstract class IterateeSuite[F[_]: Monad] extends BaseIterateeSuite[F] {
  checkAll(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadTests[VectorIntFoldingIteratee].monad[Vector[Int], Vector[Int], Vector[Int]]
  )
}

abstract class IterateeErrorSuite[F[_], T: Arbitrary: Eq](implicit F: MonadError[F, T]) extends BaseIterateeSuite[F] {
  implicit def monadError: MonadError[VectorIntFoldingIteratee, T] = Iteratee.iterateeMonadError[F, T, Vector[Int]]

  implicit val arbitraryVectorIntFoldingIteratee: Arbitrary[VectorIntFoldingIteratee[Vector[Int]]] =
    arbitraryVectorIteratee[F, Int]

  implicit val eqVectorIntIteratee: Eq[VectorIntFoldingIteratee[Vector[Int]]] =
    eqIteratee[F, Vector[Int], Vector[Int]]

  implicit val eqXorUnitIteratee: Eq[VectorIntFoldingIteratee[Xor[T, Unit]]] =
    eqIteratee[F, Vector[Int], Xor[T, Unit]]

  implicit val eqXorVectorIntIteratee: Eq[VectorIntFoldingIteratee[Xor[T, Vector[Int]]]] =
    eqIteratee[F, Vector[Int], Xor[T, Vector[Int]]]

  implicit val eqVectorInt3Iteratee: Eq[VectorIntFoldingIteratee[(Vector[Int], Vector[Int], Vector[Int])]] =
    eqIteratee[F, Vector[Int], (Vector[Int], Vector[Int], Vector[Int])]

  implicit val eqXorTVectorInt: Eq[XorT[({ type L[x] = Iteratee[F, Vector[Int], x] })#L, T, Vector[Int]]] =
    XorT.xorTEq(eqXorVectorIntIteratee)

  implicit val arbitraryVectorIntFunctionIteratee: Arbitrary[VectorIntFoldingIteratee[Vector[Int] => Vector[Int]]] =
    arbitraryFunctionIteratee[F, Vector[Int]]

  checkAll(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadErrorTests[VectorIntFoldingIteratee, T].monadError[Vector[Int], Vector[Int], Vector[Int]]
  )
}

abstract class BaseIterateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(
    minSize = 0,
    maxSize = 5000
  )

  type VectorIntProducingIteratee[E] = Iteratee[F, E, Vector[Int]]
  type VectorIntFoldingIteratee[A] = Iteratee[F, Vector[Int], A]

  implicit val isomorphisms: CartesianTests.Isomorphisms[VectorIntFoldingIteratee] =
    CartesianTests.Isomorphisms.invariant[VectorIntFoldingIteratee]

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

      eav.enumerator.run(myDrain(Nil)) === F.map(eav.enumerator.toVector)(_.toList)
    }
  }

  test("done with no leftovers") {
    check { (eav: EnumeratorAndValues[Int], s: String) =>
      eav.resultWithLeftovers(done(s)) === F.pure((s, eav.values))
    }
  }

  test("done with exactly one leftover") {
    check { (eav: EnumeratorAndValues[Int], s: String, e: Int) =>
      eav.resultWithLeftovers(done(s, Vector(e))) === F.pure((s, e +: eav.values))
    }
  }

  test("done with leftovers") {
    check { (eav: EnumeratorAndValues[Int], s: String, es: Vector[Int]) =>
      eav.resultWithLeftovers(done(s, es)) === F.pure((s, es ++ eav.values))
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

  test("consume") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val result = eav.resultWithLeftovers(consume)
      result === F.pure((eav.values, Vector.empty)) &&
      result === eav.resultWithLeftovers(identity.flatMap(_ => consume))
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

  test("dropWhile with nothing left in chunk") {
    val iteratee = for {
      _ <- dropWhile[Int](_ < 100)
      r <- consume
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
      eav.resultWithLeftovers(consume.flatMap(_ => isEnd)) === F.pure((true, Vector.empty))
    }
  }

  test("foreach") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var total = 0
      val iteratee = foreach[Int](i => total += i)
      eav.resultWithLeftovers(iteratee) === F.pure(((), Vector.empty)) && total === eav.values.sum
    }
  }

  test("foreachM") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var total = 0
      val iteratee = foreachM[Int](i => F.pure(total += i))
      eav.resultWithLeftovers(iteratee) === F.pure(((), Vector.empty)) && total === eav.values.sum
    }
  }

  test("discard") {
    check { (eav: EnumeratorAndValues[Int]) =>
      var total = 0
      val iteratee = fold[Int, Int](0) {
        case (acc, i) =>
          total += i
          i
      }
      val result = eav.values.lastOption.getOrElse(0)

      eav.resultWithLeftovers(iteratee) === F.pure((result, Vector.empty)) &&
      total === eav.values.sum
    }
  }

  test("apply") {
    check { (eav: EnumeratorAndValues[Int]) =>
      consume.apply(eav.enumerator).apply(eav.enumerator).run === F.pure(eav.values ++ eav.values)
    }
  }

  test("mapK") {
    check { (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int]) =>
      eav.enumerator.run(iteratee.mapK(F.pure)) === eav.enumerator.run(iteratee)
    }
  }

  test("contramap") {
    check { (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int]) =>
      eav.enumerator.run(iteratee.contramap(_ + 1)) === eav.enumerator.map(_ + 1).run(iteratee)
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

  test("zip where leftover sizes must be compared") {
    check { (eav: EnumeratorAndValues[Int]) =>
      val iteratee = take[Int](2).zip(take(3))

      val result = ((eav.values.take(2), eav.values.take(3)), eav.values.drop(3))

      eav.resultWithLeftovers(iteratee) === F.pure(result)
    }
  }

  test("zip with single leftovers") {
    val es = Vector(1, 2, 3, 4)
    val enumerator = enumVector(es)
    val iteratee1 = take[Int](2).zip(take(3)).zip(take(4))
    val iteratee2 = take[Int](2).zip(take(3)).zip(consume)
    val result = ((es.take(2), es.take(3)), es)

    assert(
      enumerator.run(iteratee1) === F.pure(result) && enumerator.run(iteratee2) === F.pure(result)
    )
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
      done[Int, String](s).fold(_ => None, (v, r) => Some((v, r))) === F.pure(Some((s, Vector.empty)))
    }
  }

  test("folding done with leftovers") {
    check { (s: String, es: Vector[Int]) =>
      done[Int, String](s, es).fold(_ => None, (v, r) => Some((v, r))) === F.pure(Some((s, es)))
    }
  }

  test("folding cont with one value") {
    def myDrain(acc: List[Int]): Iteratee[F, Int, List[Int]] = cont[Int, List[Int]](
      els => myDrain(acc ::: (els.head :: els.tail.toList)),
      F.pure(acc)
    )

    check { (es: List[Int]) =>
      val folded = myDrain(es).fold[F[List[Int]]](_(NonEmptyVector(0)).run, (_, _) => F.pure(Nil))

      F.flatten(folded) === F.pure(es :+ 0)
    }
  }

  test("folding cont with multiple values") {
    def myDrain(acc: List[Int]): Iteratee[F, Int, List[Int]] = cont[Int, List[Int]](
      els => myDrain(acc ::: (els.head :: els.tail.toList)),
      F.pure(acc)
    )

    check { (es: List[Int]) =>
      val folded = myDrain(es).fold[F[List[Int]]](_(NonEmptyVector(0, Vector(1, 2, 3))).run, (_, _) => F.pure(Nil))

      F.flatten(folded) === F.pure(es ++ Vector(0, 1, 2, 3))
    }
  }

  /**
   * Well-behaved iteratees don't inject values into the stream, but if we do
   * end up in this situation, we try to make sure something fairly reasonable
   * happens (and specifically that flatMap stays associative in as many cases
   * as possible).
   */
  test("successive iteratees that inject values") {
    check { (l1: Vector[Int], l2: Vector[Int]) =>
      val allL1I = done((), l1)
      val allL2I = done((), l2)
      val oneL1I = done((), l1.take(1))
      val oneL2I = done((), l2.take(1))

      val iteratee1: Iteratee[F, Int, Vector[Int]] = allL1I.flatMap(_ => allL2I).flatMap(_ => consume)
      val iteratee2: Iteratee[F, Int, Vector[Int]] = allL1I.flatMap(_ => allL2I.flatMap(_ => consume))

      val iteratee3: Iteratee[F, Int, Vector[Int]] = allL1I.flatMap(_ => oneL2I).flatMap(_ => consume)
      val iteratee4: Iteratee[F, Int, Vector[Int]] = allL1I.flatMap(_ => oneL2I.flatMap(_ => consume))

      val iteratee5: Iteratee[F, Int, Vector[Int]] = oneL1I.flatMap(_ => allL2I).flatMap(_ => consume)
      val iteratee6: Iteratee[F, Int, Vector[Int]] = oneL1I.flatMap(_ => allL2I.flatMap(_ => consume))

      val iteratee7: Iteratee[F, Int, Vector[Int]] = oneL1I.flatMap(_ => oneL2I).flatMap(_ => consume)
      val iteratee8: Iteratee[F, Int, Vector[Int]] = oneL1I.flatMap(_ => oneL2I.flatMap(_ => consume))

      iteratee1 === iteratee2 && iteratee3 === iteratee4 && iteratee5 === iteratee6 && iteratee7 === iteratee8
    }
  }
}
