package io.iteratee

import algebra.Eq
import cats.functor.Contravariant
import cats.laws.ContravariantLaws
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.typelevel.discipline.Laws

trait ContravariantTests[F[_]] extends Laws with InvariantTests[F] {
  def laws: ContravariantLaws[F]

  def contravariant[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "contravariant",
      parent = None,
      "contravariant identity" -> Prop.forAll(laws.contravariantIdentity[A] _),
      "contravariant composition" -> Prop.forAll(laws.contravariantComposition[A, B, C] _))
  }
}

object ContravariantTests {
  def apply[F[_]: Contravariant]: ContravariantTests[F] =
    new ContravariantTests[F] { def laws: ContravariantLaws[F] = ContravariantLaws[F] }
}
