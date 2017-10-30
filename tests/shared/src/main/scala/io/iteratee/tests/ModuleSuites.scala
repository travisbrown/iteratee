package io.iteratee.tests

import cats.Eval
import cats.data.EitherT
import cats.instances.either.catsStdEqForEither
import cats.instances.option.catsKernelStdEqForOption
import cats.instances.try_.catsStdEqForTry
import cats.kernel.Eq
import io.iteratee.modules._
import io.iteratee.testing.EqInstances.eqThrowable
import org.scalacheck.Arbitrary
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

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
        20.seconds
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
