package io.iteratee.files

import cats.ApplicativeError
import cats.data.Xor
import scala.util.Try
import scala.util.control.NonFatal

trait EffectCapture[F[_]] {
  def apply[A](a: => A): F[A]
}

final object EffectCapture {
  def fromApplicativeError[F[_]](implicit F: ApplicativeError[F, Throwable]): EffectCapture[F] =
    new EffectCapture[F] {
      def apply[A](a: => A): F[A] = F.catchNonFatal(a)
    }

  implicit val tryEffectCapture: EffectCapture[Try] = new EffectCapture[Try] {
    def apply[A](a: => A): Try[A] = Try(a)
  }

  implicit val eitherEffectCapture: EffectCapture[({ type L[x] = Either[Throwable, x] })#L] =
    new EffectCapture[({ type L[x] = Either[Throwable, x] })#L] {
      def apply[A](a: => A): Either[Throwable, A] = try Right(a) catch {
        case NonFatal(e) => Left(e)
      }
    }

  implicit val xorEffectCapture: EffectCapture[({ type L[x] = Xor[Throwable, x] })#L] =
    new EffectCapture[({ type L[x] = Xor[Throwable, x] })#L] {
      def apply[A](a: => A): Xor[Throwable, A] = try Xor.right(a) catch {
        case NonFatal(e) => Xor.left(e)
      }
    }
}
