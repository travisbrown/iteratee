package io.iteratee.testing

import cats.{ Eq, Eval, Monad, MonadError }
import cats.data.{ EitherT, NonEmptyList }
import cats.laws.discipline.{ CartesianTests, ContravariantTests, MonadTests, MonadErrorTests }
import io.iteratee.{
  EnumerateeModule,
  EnumeratorModule,
  Iteratee,
  IterateeErrorModule,
  IterateeModule,
  Module
}
import io.iteratee.internal.Step
import org.scalacheck.{ Arbitrary, Cogen }
import scala.Predef._

abstract class IterateeSuite[F[_]: Monad] extends BaseIterateeSuite[F] {
  this: EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] with Module[F] =>

  checkLaws(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadTests[VectorIntFoldingIteratee].monad[Vector[Int], Vector[Int], Vector[Int]]
  )
}

abstract class IterateeErrorSuite[F[_], T: Arbitrary: Eq: Cogen](implicit
  MEF: MonadError[F, T]
) extends BaseIterateeSuite[F] {
  this: EnumerateeModule[F] with EnumeratorModule[F] with IterateeErrorModule[F, T]
    with Module[F] { type M[f[_]] = MonadError[f, T] } =>

  implicit val monadError: MonadError[VectorIntFoldingIteratee, T] = Iteratee.iterateeMonadError[F, T, Vector[Int]]

  implicit val arbitraryVectorIntFoldingIteratee: Arbitrary[VectorIntFoldingIteratee[Vector[Int]]] =
    arbitraryVectorIteratee[F, Int]

  implicit val eqVectorIntIteratee: Eq[VectorIntFoldingIteratee[Vector[Int]]] =
    eqIteratee[F, Vector[Int], Vector[Int]]

  implicit val eqEitherUnitIteratee: Eq[VectorIntFoldingIteratee[Either[T, Unit]]] =
    eqIteratee[F, Vector[Int], Either[T, Unit]]

  implicit val eqEitherVectorIntIteratee: Eq[VectorIntFoldingIteratee[Either[T, Vector[Int]]]] =
    eqIteratee[F, Vector[Int], Either[T, Vector[Int]]]

  implicit val eqVectorInt3Iteratee: Eq[VectorIntFoldingIteratee[(Vector[Int], Vector[Int], Vector[Int])]] =
    eqIteratee[F, Vector[Int], (Vector[Int], Vector[Int], Vector[Int])]

  implicit val eqEitherTVectorInt: Eq[EitherT[({ type L[x] = Iteratee[F, Vector[Int], x] })#L, T, Vector[Int]]] =
    EitherT.catsDataEqForEitherT(eqEitherVectorIntIteratee)

  implicit val arbitraryVectorIntFunctionIteratee: Arbitrary[VectorIntFoldingIteratee[Vector[Int] => Vector[Int]]] =
    arbitraryFunctionIteratee[F, Vector[Int]]

  checkLaws(
    s"Iteratee[$monadName, Vector[Int], Vector[Int]]",
    MonadErrorTests[VectorIntFoldingIteratee, T].monadError[Vector[Int], Vector[Int], Vector[Int]]
  )

  "ensureEval" should "be executed when the iteratee is done" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var done = false

    val iteratee = consume[Int].ensureEval(Eval.always(F.pure(done = true)))

    assert(!done)
    assert(eav.resultWithLeftovers(iteratee) === F.pure((eav.values, Vector.empty)))
    assert(done)
  }
}

abstract class BaseIterateeSuite[F[_]: Monad] extends ModuleSuite[F] {
  this: EnumerateeModule[F] with EnumeratorModule[F] with IterateeModule[F] with Module[F] =>

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
    minSize = 0,
    sizeRange = 5000
  )

  type VectorIntProducingIteratee[E] = Iteratee[F, E, Vector[Int]]
  type VectorIntFoldingIteratee[A] = Iteratee[F, Vector[Int], A]

  implicit val isomorphisms: CartesianTests.Isomorphisms[VectorIntFoldingIteratee] =
    CartesianTests.Isomorphisms.invariant[VectorIntFoldingIteratee]

  def myDrain(acc: List[Int]): Iteratee[F, Int, List[Int]] = cont[Int, List[Int]](
    els => myDrain(acc ::: els.toList),
    F.pure(acc)
  )

  checkLaws(
    s"Iteratee[$monadName, Int, Vector[Int]]",
    ContravariantTests[VectorIntProducingIteratee].contravariant[Vector[Int], Int, Vector[Int]]
  )

  checkLaws(
    s"Iteratee[$monadName, Int, Vector[Int]]",
    ContravariantTests[VectorIntProducingIteratee].invariant[Vector[Int], Int, Vector[Int]]
  )

  "cont" should "work recursively in an iteratee returning a list" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.enumerator.into(myDrain(Nil)) === F.map(eav.enumerator.toVector)(_.toList))
  }

  it should "work with fold with one value" in forAll { (es: List[Int]) =>
    val folded = myDrain(es).fold[F[List[Int]]](_(NonEmptyList(0, Nil)).run, (_, _) => F.pure(Nil))

    assert(F.flatten(folded) === F.pure(es :+ 0))
  }

  it should "work with fold with multiple values" in forAll { (es: List[Int]) =>
    val folded = myDrain(es).fold[F[List[Int]]](_(NonEmptyList(0, List(1, 2, 3))).run, (_, _) => F.pure(Nil))

    assert(F.flatten(folded) === F.pure(es ++ Vector(0, 1, 2, 3)))
  }

  "done" should "work correctly" in forAll { (eav: EnumeratorAndValues[Int], s: String) =>
    assert(eav.resultWithLeftovers(done(s)) === F.pure((s, eav.values)))
  }

  it should "work with fold with no leftovers" in forAll { (s: String) =>
    assert(done[Int, String](s).fold(_ => None, (v, r) => Some((v, r))) === F.pure(Some((s, Nil))))
  }

  "Step.doneWithLeftovers" should "do something vaguely reasonable with exactly one leftover" in {
    forAll { (eav: EnumeratorAndValues[Int], s: String, e: Int) =>
      val iteratee = Iteratee.fromStep(Step.doneWithLeftovers(s, List(e)))

      assert(eav.resultWithLeftovers(iteratee) === F.pure((s, e +: eav.values)))
    }
  }

  it should "do something vaguely reasonable with leftovers" in {
    forAll { (eav: EnumeratorAndValues[Int], s: String, es: List[Int]) =>
      val iteratee = Iteratee.fromStep(Step.doneWithLeftovers(s, es))

      assert(eav.resultWithLeftovers(iteratee) === F.pure((s, es.toVector ++ eav.values)))
    }
  }

  it should "do something vaguely reasonable with fold with leftovers" in forAll { (s: String, es: List[Int]) =>
    val iteratee = Iteratee.fromStep(Step.doneWithLeftovers[F, Int, String](s, es))

    assert(iteratee.fold(_ => None, (v, r) => Some((v, r))) === F.pure(Some((s, es))))
  }

  "liftToIteratee" should "lift a value in a context into an iteratee" in forAll { (i: Int) =>
    assert(liftToIteratee(F.pure(i)).run === F.pure(i))
  }

  it should "lift a value in a context when run on an enumerator" in forAll { (eav: EnumeratorAndValues[Int], i: Int) =>
    assert(eav.enumerator.into(liftToIteratee(F.pure(i))) === F.pure(i))
  }

  "liftMEval" should "lift a value in a context into an iteratee" in forAll { (i: Int) =>
    var counter = 0
    val eval = Eval.later {
      counter += i
      F.pure(i)
    }

    assert(counter === 0)
    assert(Iteratee.liftMEval(eval).run === F.pure(i))
    assert(counter === i)
  }

  it should "lift a value in a context when run on an enumerator" in forAll { (eav: EnumeratorAndValues[Int], i: Int) =>
    var counter = 0
    val eval = Eval.later {
      counter += i
      F.pure(i)
    }

    assert(counter === 0)
    assert(eav.enumerator.into(Iteratee.liftMEval(eval)) === F.pure(i))
    assert(counter === i)
  }

  "identityIteratee" should "consume no input" in forAll { (eav: EnumeratorAndValues[Int], it: Iteratee[F, Int, Int]) =>
    assert(eav.resultWithLeftovers(identityIteratee) === F.pure(((), eav.values)))
    assert(eav.resultWithLeftovers(identityIteratee.flatMap(_ => it)) === eav.resultWithLeftovers(it))
  }

  "consume" should "consume the entire stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = eav.resultWithLeftovers(consume)

    assert(result === F.pure((eav.values, Vector.empty)))
    assert(result === eav.resultWithLeftovers(identityIteratee.flatMap(_ => consume)))
  }

  "consumeIn" should "consume the entire stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(consumeIn[Int, List]) === F.pure((eav.values.toList, Vector.empty)))
  }

  "reversed" should "consume and reverse the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(reversed) === F.pure((eav.values.toList.reverse, Vector.empty)))
  }

  "head" should "consume and return the first value" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = (eav.values.headOption, eav.values.drop(1))

    assert(eav.resultWithLeftovers(head[Int]) === F.pure(result))
  }

  "peek" should "consume the first value without consuming it" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = (eav.values.headOption, eav.values)

    assert(eav.resultWithLeftovers(peek[Int]) === F.pure(result))
  }

  "last" should "return the last value" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = (eav.values.lastOption, Vector.empty[Int])

    assert(eav.resultWithLeftovers(last[Int]) === F.pure(result))
  }

  "takeI" should "consume the specified number of values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    /**
      * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
      * ScalaCheck is likely to run into.
      */
    whenever(n != Int.MaxValue) {
      assert(eav.resultWithLeftovers(takeI[Int](n)) === F.pure((eav.values.take(n), eav.values.drop(n))))
    }
  }

  "takeWhileI" should "consume the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    assert(eav.resultWithLeftovers(takeWhileI(_ < n)) === F.pure(eav.values.span(_ < n)))
  }

  "dropI" should "drop the specified number of values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    /**
     * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
     * ScalaCheck is likely to run into.
      */
    whenever(n != Int.MaxValue) {
      assert(eav.resultWithLeftovers(dropI[Int](n)) === F.pure(((), eav.values.drop(n))))
    }
  }

  "dropWhileI" should "drop the specified values" in forAll { (eav: EnumeratorAndValues[Int], n: Int) =>
    assert(eav.resultWithLeftovers(dropWhileI(_ < n)) === F.pure(((), eav.values.dropWhile(_ < n))))
  }

  it should "drop the specified values with nothing left in chunk" in {
    val iteratee = for {
      _ <- dropWhileI[Int](_ < 100)
      r <- consume
    } yield r

    assert(enumVector(Vector(1, 2, 3)).into(iteratee) === F.pure(Vector.empty))
  }

  "fold" should "collapse the stream into a value" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(fold[Int, Int](0)(_ + _)) === F.pure((eav.values.sum, Vector.empty)))
  }

  "foldM" should "effectfully collapse the stream into a value" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = (eav.values.sum, Vector.empty)

    assert(eav.resultWithLeftovers(foldM[Int, Int](0)((acc, i) => F.pure(acc + i))) === F.pure(result))
  }

  "length" should "return the length of the stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(length) === F.pure((eav.values.size.toLong, Vector.empty)))
  }

  "sum" should "return the sum of a stream of integers" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(sum) === F.pure((eav.values.sum, Vector.empty)))
  }

  "isEnd" should "indicate whether a stream has ended" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(eav.resultWithLeftovers(isEnd) === F.pure((eav.values.isEmpty, eav.values)))
    assert(eav.resultWithLeftovers(consume.flatMap(_ => isEnd)) === F.pure((true, Vector.empty)))
  }

  "foreach" should "perform an operation on all values in a stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var total = 0
    val iteratee = foreach[Int](i => total += i)

    assert(eav.resultWithLeftovers(iteratee) === F.pure(((), Vector.empty)) && total === eav.values.sum)
  }

  "foreach" should "perform an operation on all values in a grouped stream" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val eavg = EnumeratorAndValues(eav.enumerator.grouped(3), eav.values.grouped(3).toVector)

      var total = 0
      val iteratee = foreach[Vector[Int]](is => total += is.sum)

      assert(eavg.resultWithLeftovers(iteratee) === F.pure(((), Vector.empty)) && total === eavg.values.flatten.sum)
    }
  }

  "foreachM" should "perform an effectful operation on all values in a stream" in {
    forAll { (eav: EnumeratorAndValues[Int]) =>
      var total = 0
      val iteratee = foreachM[Int](i => F.pure(total += i))

      assert(eav.resultWithLeftovers(iteratee) === F.pure(((), Vector.empty)) && total === eav.values.sum)
    }
  }

  "discard" should "throw away the result" in forAll { (eav: EnumeratorAndValues[Int]) =>
    var total = 0
    val iteratee = fold[Int, Int](0) {
      case (acc, i) =>
        total += i
        i
    }

    assert(eav.resultWithLeftovers(iteratee.discard) === F.pure(((), Vector.empty)))
    assert(total === eav.values.sum)
  }

  "apply" should "process the values in a stream" in forAll { (eav: EnumeratorAndValues[Int]) =>
    assert(consume.apply(eav.enumerator).apply(eav.enumerator).run === F.pure(eav.values ++ eav.values))
  }

  "flatMapM" should "apply an effectful function" in {
    forAll { (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int]) =>
      assert(eav.enumerator.into(iteratee.flatMapM(F.pure)) === eav.enumerator.into(iteratee))
    }
  }

  "contramap" should "apply a function on incoming values" in {
    forAll { (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int]) =>
      assert(eav.enumerator.into(iteratee.contramap(_ + 1)) === eav.enumerator.map(_ + 1).into(iteratee))
    }
  }

  "through" should "pipe incoming values through an enumeratee" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = (eav.values.sum + eav.values.size, Vector.empty)

    assert(eav.resultWithLeftovers(sum[Int].through(map(_ + 1))) === F.pure(result))
  }

  "as" should "replace the result" in {
    forAll { (eav: EnumeratorAndValues[Int], iteratee: Iteratee[F, Int, Int], value: Int) =>
      assert(eav.enumerator.into(iteratee.as(value)) === F.pure(value))
    }
  }

  "zip" should "zip two iteratees" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = ((eav.values.sum, eav.values.size.toLong), Vector.empty)

    assert(eav.resultWithLeftovers(sum[Int].zip(length)) === F.pure(result))
  }

  it should "zip two iteratees with leftovers (scalaz/scalaz#1068)" in {
    forAll { (eav: EnumeratorAndValues[Int], m: Int, n: Int) =>
      /**
       * This isn't a comprehensive way to avoid SI-9581, but it seems to keep clear of the cases
       * ScalaCheck is likely to run into.
       */
      whenever(m != Int.MaxValue && n != Int.MaxValue) {
        val result = ((eav.values.take(m), eav.values.take(n)), eav.values.drop(math.max(m, n)))

        assert(eav.resultWithLeftovers(takeI[Int](m).zip(takeI[Int](n))) === F.pure(result))
      }
    }
  }

  it should "zip two iteratees where leftover sizes must be compared" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val iteratee = takeI[Int](2).zip(takeI(3))
    val result = ((eav.values.take(2), eav.values.take(3)), eav.values.drop(3))

    assert(eav.resultWithLeftovers(iteratee) === F.pure(result))
  }

  it should "zip two iteratees where only one has leftovers" in forAll { (v: Vector[Int], n: Int) =>
    val taken = n % (v.size + 1)
    val enumerator = enumVector(v)
    val iteratee1 = takeI[Int](v.size).zip(takeI(taken)).flatMap(r => consume.map((r, _)))
    val iteratee2 = takeI[Int](taken).zip(takeI(v.size)).flatMap(r => consume.map((r, _)))

    val result1 = (v, v.take(taken))
    val result2 = (v.take(taken), v)

    assert(enumerator.into(iteratee1) === F.pure((result1, Vector.empty)))
    assert(enumerator.into(iteratee2) === F.pure((result2, Vector.empty)))
  }

  it should "zip two iteratees with single leftovers" in {
    val es = Vector(1, 2, 3, 4)
    val enumerator = enumVector(es)
    val iteratee1 = takeI[Int](2).zip(takeI(3)).zip(takeI(4))
    val iteratee2 = takeI[Int](2).zip(takeI(3)).zip(consume)
    val result = ((es.take(2), es.take(3)), es)

    assert(enumerator.into(iteratee1) === F.pure(result))
    assert(enumerator.into(iteratee2) === F.pure(result))
  }

  "foldMap" should "sum a stream while transforming it" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = F.pure((eav.values.sum + eav.values.size, Vector.empty[Int]))

    assert(eav.resultWithLeftovers(foldMap(_ + 1)) === result)
  }

  "foldMapM" should "sum a stream while transforming it" in forAll { (eav: EnumeratorAndValues[Int]) =>
    val result = F.pure((eav.values.sum + eav.values.size, Vector.empty[Int]))

    assert(eav.resultWithLeftovers(foldMapM(e => F.pure(e + 1))) === result)
  }

  "foldMapOption" should "sum a stream while transforming it" in
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val result = F.pure((
        if (eav.values.isEmpty) None else Some(eav.values.sum + eav.values.size),
        Vector.empty[Int]
      ))

      assert(eav.resultWithLeftovers(foldMapOption(_ + 1)) === result)
   }

  "foldMapMOption" should "sum a stream while transforming it" in
    forAll { (eav: EnumeratorAndValues[Int]) =>
      val result = F.pure((
        if (eav.values.isEmpty) None else Some(eav.values.sum + eav.values.size),
        Vector.empty[Int]
      ))

      assert(eav.resultWithLeftovers(foldMapMOption(e => F.pure(e + 1))) === result)
   }

  "intoIteratee" should "be available on values in a context" in forAll { (i: Int) =>
    import syntax._

    assert(F.pure(i).intoIteratee.run === F.pure(i))
  }
}
