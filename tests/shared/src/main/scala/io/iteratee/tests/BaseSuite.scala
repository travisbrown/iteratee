package io.iteratee.tests

import cats.{ Eq, Eval }
import cats.data.{ Xor, XorT }
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import io.iteratee._
import org.scalatest.FlatSpec
import org.scalatest.prop.{ Checkers, GeneratorDrivenPropertyChecks }
import org.typelevel.discipline.Laws

class BaseSuite extends FlatSpec with GeneratorDrivenPropertyChecks
  with AllInstances with AllSyntax
  with ArbitraryInstances with EqInstances {
  override def convertToEqualizer[T](left: T): Equalizer[T] =
    sys.error("Intentionally ambiguous implicit for Equalizer")

  def checkLaws(name: String, ruleSet: Laws#RuleSet): Unit = ruleSet.all.properties.zipWithIndex.foreach {
    case ((id, prop), 0) => name should s"obey $id" in Checkers.check(prop)
    case ((id, prop), _) => it should s"obey $id" in Checkers.check(prop)
  }
}

abstract class ModuleSuite[F[_]] extends BaseSuite with ArbitraryEnumerators[F] {
  this: Module[F] with EnumeratorModule[F] with IterateeModule[F] =>

  def monadName: String
  implicit def eqF[A: Eq]: Eq[F[A]]
}

trait PureSuite extends PureModule {
  def monadName: String = "Id"

  implicit def eqF[A](implicit A: Eq[A]): Eq[A] = A
}

trait EvalSuite extends EvalModule {
  def monadName: String = "Eval"

  implicit def eqF[A: Eq]: Eq[Eval[A]] = Eval.catsEqForEval[A]
}

trait XorSuite extends XorModule {
  def monadName: String = "XorT[Eval, Throwable, ?]"

  implicit def eqEval[A](implicit A: Eq[A]): Eq[Eval[Xor[Throwable, A]]] =
    Eval.catsEqForEval(Xor.catsDataEqForXor(Eq.fromUniversalEquals, A))

  implicit def eqF[A](implicit A: Eq[A]): Eq[XorT[Eval, Throwable, A]] = XorT.catsDataEqForXorT(eqEval(A))
}
