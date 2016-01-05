package io.iteratee.internal

import cats.{ Applicative, Monad }
import cats.data.NonEmptyVector
import cats.arrow.NaturalTransformation

private[internal] abstract class BaseDone[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] {
  def value: A
  final def isDone: Boolean = true
  final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
  final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(this)
  final def end: F[Step.Ended[F, E, A]] = F.pure(new Step.Ended(value))
  final def run: F[A] = F.map(end)(_.value)
}

private[internal] case class NoLeftovers[F[_]: Applicative, E, A](value: A) extends BaseDone[F, E, A] {
  final def fold[Z](
    ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
    ifDone: (A, Vector[E]) => Z,
    ifEnd: A => Z
  ): Z = ifDone(value, Vector.empty)

  final def map[B](f: A => B): Step[F, E, B] = new NoLeftovers(f(value))
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = new NoLeftovers(value)
  final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = new NoLeftovers(value)
  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = f(value)
  final def zip[B](other: Step[F, E, B])(implicit M: Monad[F]): F[Step[F, E, (A, B)]] =
    M.pure(other.map((value, _)))
}

private[internal] case class WithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Input[E])
  extends BaseDone[F, E, A] {
  final def fold[Z](
    ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
    ifDone: (A, Vector[E]) => Z,
    ifEnd: A => Z
  ): Z = ifDone(value, remaining.toVector)

  final def map[B](f: A => B): Step[F, E, B] = new WithLeftovers(f(value), remaining)
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = new NoLeftovers(value)
  final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] =
    new WithLeftovers(value, remaining)

  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
    M.flatMap(f(value)) {
      case ended @ Step.Ended(_) => M.pure(ended)
      case NoLeftovers(otherValue) => M.pure(new WithLeftovers(otherValue, remaining))
      case WithLeftovers(otherValue, otherRemaining) => M.pure(
        new WithLeftovers(otherValue, otherRemaining.append(remaining))
      )
      case Step.Done(v) => M.pure(new WithLeftovers(v, remaining))
      case step => remaining.foldWith(
        new Input.Folder[E, F[Step[F, E, B]]] {
          def onEl(e: E): F[Step[F, E, B]] = step.feedEl(e)
          def onChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = step.feedChunk(h1, h2, t)
        }
      )
    }

  final def zip[B](other: Step[F, E, B])(implicit M: Monad[F]): F[Step[F, E, (A, B)]] = M.pure(
    other match {
      case NoLeftovers(otherValue) => new NoLeftovers((value, otherValue))
      case WithLeftovers(otherValue, otherRemaining) => new WithLeftovers(
        (value, otherValue),
        if (remaining.size <= otherRemaining.size) remaining else otherRemaining
      )
      case Step.Ended(otherValue) => new Step.Ended((value, otherValue))
      case step => step.map((value, _))
    }
  )
}
