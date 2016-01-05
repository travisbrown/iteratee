package io.iteratee.internal

import cats.{ Applicative, Monad }
import cats.data.NonEmptyVector
import cats.arrow.NaturalTransformation

abstract class Done[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] {
  def value: A
  final def isDone: Boolean = true
  final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
  final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(this)
  final def end: F[Done.Ended[F, E, A]] = F.pure(new Done.Ended(value))
  final def run: F[A] = F.map(end)(_.value)
}

final object Done {
  final def unapply[F[_], E, A](step: Step[F, E, A]): Option[A] =
    if (step.isDone) Some(step.asInstanceOf[Done[F, E, A]].value) else None

  private[internal] case class NoLeftovers[F[_]: Applicative, E, A](value: A) extends Done[F, E, A] {
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
    extends Done[F, E, A] {
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
        case Done(v) => M.pure(new WithLeftovers(v, remaining))
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
        case Ended(otherValue) => new Ended((value, otherValue))
        case step => step.map((value, _))
      }
    )
  }

  case class Ended[F[_]: Applicative, E, A](value: A) extends Done[F, E, A] {
    final def fold[Z](
      ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
      ifDone: (A, Vector[E]) => Z,
      ifEnd: A => Z
    ): Z = ifEnd(value)

    final def map[B](f: A => B): Step[F, E, B] = endedMap(f)
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = endedContramap(f)
    final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = endedMapI(f)
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      Ended.asStep(endedBind(f))

    final def zip[B](other: Step[F, E, B])(implicit M: Monad[F]): F[Step[F, E, (A, B)]] =
      other match {
        case Done(otherValue) => M.pure(new Ended((value, otherValue)))
        case step => M.map(step.end)(endedZip)
      }

    final def endedMap[B](f: A => B): Ended[F, E, B] = new Ended(f(value))
    final def endedContramap[E2](f: E2 => E): Ended[F, E2, A] = new Ended(value)
    final def endedMapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Ended[G, E, A] = new Ended(value)
    final def endedBind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Ended[F, E, B]] =
      M.flatMap(f(value)) {
        case Done(v) => M.pure(new Ended(v))
        case step => step.end
      }

    final def endedZip[B](other: Ended[F, E, B]): Ended[F, E, (A, B)] =
      new Ended((value, other.value))
  }

  final object Ended {
    /**
     * Up-cast an [[Ended]] in a context to a [[Step]] in a context.
     */
    final def asStep[F[_], E, A](ended: F[Ended[F, E, A]]): F[Step[F, E, A]] =
      ended.asInstanceOf[F[Step[F, E, A]]]
  }
}
