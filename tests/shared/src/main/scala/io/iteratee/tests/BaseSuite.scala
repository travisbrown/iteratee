package io.iteratee.tests

import cats.{ Eq, Eval }
import cats.data.EitherT
import cats.instances.AllInstances
import cats.instances.either.catsStdEqForEither
import cats.instances.option.catsKernelStdEqForOption
import cats.instances.try_.catsStdEqForTry
import cats.syntax.AllSyntax
import io.iteratee._
import io.iteratee.modules._
import org.scalacheck.Arbitrary
import org.scalatest.FlatSpec
import org.scalatest.prop.{ Checkers, GeneratorDrivenPropertyChecks }
import org.typelevel.discipline.Laws
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

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

trait EitherSuite extends EitherModule {
  def monadName: String = "Either[Throwable, ?]"

  implicit def eqF[A](implicit A: Eq[A]): Eq[Either[Throwable, A]] = catsStdEqForEither(Eq.fromUniversalEquals, A)
}

trait EitherTSuite extends EitherTModule {
  def monadName: String = "EitherT[Eval, Throwable, ?]"

  implicit def eqEval[A](implicit A: Eq[A]): Eq[Eval[Either[Throwable, A]]] =
    Eval.catsEqForEval(catsStdEqForEither(Eq.fromUniversalEquals, A))

  implicit def eqF[A](implicit A: Eq[A]): Eq[EitherT[Eval, Throwable, A]] = EitherT.catsDataEqForEitherT(eqEval(A))
}

trait EvalSuite extends EvalModule {
  def monadName: String = "Eval"

  implicit def eqF[A: Eq]: Eq[Eval[A]] = Eval.catsEqForEval[A]
}

trait FutureSuite extends FutureModule {
  def monadName: String = "Future"

  protected def ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit def eqF[A](implicit A: Eq[A]): Eq[Future[A]] = new Eq[Future[A]] {
    def liftToTry[A](f: Future[A]): Future[Try[A]] = f.map(Success(_)).recover {
      case t => Failure(t)
    }

    def eqv(fx: Future[A], fy: Future[A]): Boolean =
      Await.result(
        liftToTry(fx).zip(liftToTry(fy)).map {
          case (tx, ty) => Eq[Try[A]].eqv(tx, ty)
        },
        2.seconds
      )
  }
}

object FutureSuite {
  /**
   * Needed because `scala.concurrent.Future` boxes `java.lang.Error`.
   */
  implicit val arbitraryNonFatalThrowable: Arbitrary[Throwable] =
    Arbitrary(Arbitrary.arbitrary[Exception].map(identity))
}

trait IdSuite extends IdModule {
  def monadName: String = "Id"

  implicit def eqF[A](implicit A: Eq[A]): Eq[A] = A
}

trait OptionSuite extends OptionModule {
  def monadName: String = "Option"

  implicit def eqF[A](implicit A: Eq[A]): Eq[Option[A]] = catsKernelStdEqForOption
}

trait TrySuite extends TryModule {
  def monadName: String = "Try"

  implicit def eqF[A](implicit A: Eq[A]): Eq[Try[A]] = catsStdEqForTry(A, Eq.fromUniversalEquals)

}

