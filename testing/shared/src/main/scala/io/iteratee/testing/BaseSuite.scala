package io.iteratee.testing

import cats.instances.AllInstances
import cats.kernel.Eq
import cats.syntax.AllSyntax
import io.iteratee.modules.{EnumeratorModule, IterateeModule, Module}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}
import org.typelevel.discipline.Laws

class BaseSuite
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with AllInstances
    with AllSyntax
    with ArbitraryInstances
    with EqInstances {
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
