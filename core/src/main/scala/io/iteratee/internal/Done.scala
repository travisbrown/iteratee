package io.iteratee.internal

import cats.{ Applicative, Monad }
import cats.data.NonEmptyVector
import cats.arrow.FunctionK

private[internal] case class Done[F[_], E, A](
  value: A,
  remaining: Vector[E]
)(implicit F: Applicative[F]) extends Step[F, E, A] {
  final def isDone: Boolean = true
  final def run: F[A] = F.pure(value)
  final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
  final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(this)

  final def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z =
    ifDone(value, remaining)

  final def map[B](f: A => B): Step[F, E, B] = Done(f(value), remaining)
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = Done(value, Vector.empty)
  final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = Done(value, remaining)

  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
    M.flatMap(f(value)) {
      case Done(otherValue, otherRemaining) => F.pure(Done(otherValue, otherRemaining ++ remaining))
      case step => remaining match {
        case xs if xs.isEmpty => f(value)
        case xs if xs.size == 1 => step.feedEl(xs.head)
        case h1 +: h2 +: t => step.feedChunk(h1, h2, t)
      }
    }

  final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = other match {
    case Done(otherValue, otherRemaining) => Done(
      (value, otherValue),
      if (remaining.size <= otherRemaining.size) remaining else otherRemaining
    )
    case step => step.map((value, _))
  }
}
