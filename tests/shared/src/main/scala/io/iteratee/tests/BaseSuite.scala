package io.iteratee.tests

import algebra.Eq
import cats.{ Eval, Id, Monad }
import cats.data.{ Xor, XorT }
import cats.std.AllInstances
import cats.syntax.AllSyntax
import io.iteratee.Module
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline

class BaseSuite extends FunSuite with Checkers with Discipline
  with ArbitraryInstances with EqInstances
  with AllInstances with AllSyntax {
  override def convertToEqualizer[T](left: T): Equalizer[T] =
    sys.error("Intentionally ambiguous implicit for Equalizer")
}

abstract class ModuleSuite[F[_]](implicit val F: Monad[F]) extends BaseSuite
  with Module[F] with ArbitraryEnumerators[F] {
  def monadName: String

  implicit def eqF[A: Eq]: Eq[F[A]]
}

trait PureSuite { this: ModuleSuite[Id] =>
  def monadName: String = "Id"

  implicit def eqF[A](implicit A: Eq[A]): Eq[A] = A
}

trait EvalSuite { this: ModuleSuite[Eval] =>
  def monadName: String = "Eval"

  implicit def eqF[A: Eq]: Eq[Eval[A]] = Eval.evalEq
}

trait XorSuite { this: ModuleSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] =>
  def monadName: String = "XorT[Eval, Throwable, ?]"

  implicit def eqEval[A](implicit A: Eq[A]): Eq[Eval[Xor[Throwable, A]]] =
    Eval.evalEq(Xor.xorEq(Eq.fromUniversalEquals, A))

  implicit def eqF[A](implicit A: Eq[A]): Eq[XorT[Eval, Throwable, A]] = XorT.xorTEq(eqEval(A))
}
