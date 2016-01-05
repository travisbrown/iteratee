package io.iteratee.internal

import cats.{ Applicative, Monad }
import cats.data.{ NonEmptyVector, OneAnd }
import cats.arrow.NaturalTransformation

abstract class Cont[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] { self =>
  final def fold[Z](
    ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z,
    ifDone: (A, Vector[E]) => Z,
    ifEnd: A => Z
  ): Z = ifCont {
    case OneAnd(e, Vector()) => feedEl(e)
    case OneAnd(h1, h2 +: t) => feedChunk(h1, h2, t)
  }
  final def isDone: Boolean = false
  final def run: F[A] = F.map(end)(_.value)

  final def mapI[G[_]: Applicative](f: NaturalTransformation[F, G]): Step[G, E, A] = new Cont.Effectful[G, E, A] {
    final def feedEl(e: E): G[Step[G, E, A]] = f(F.map(self.feedEl(e))(_.mapI(f)))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): G[Step[G, E, A]] = f(F.map(self.feedChunk(h1, h2, t))(_.mapI(f)))
    final def end: G[Done.Ended[G, E, A]] = f(F.map(self.end)(_.endedMapI(f)))
  }
  final def zip[B](other: Step[F, E, B])(implicit M: Monad[F]): F[Step[F, E, (A, B)]] = F.pure(
    other match {
      case Done(otherValue) => map((_, otherValue))
      case step => new Cont.Effectful[F, E, (A, B)] {
        final def feedEl(e: E): F[Step[F, E, (A, B)]] = M.flatten(M.map2(self.feedEl(e), step.feedEl(e))(_.zip(_)))
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, (A, B)]] =
          M.flatten(M.map2(self.feedChunk(h1, h2, t), step.feedChunk(h1, h2, t))(_.zip(_)))
        final def end: F[Done.Ended[F, E, (A, B)]] = M.map2(self.end, step.end)(_.endedZip(_))
      }
    }
  )
}

final object Cont {
  private[internal] abstract class Effectful[F[_], E, A](implicit F: Applicative[F]) extends Cont[F, E, A] { self =>
    final def map[B](f: A => B): Step[F, E, B] = new Effectful[F, E, B] {
      final def feedEl(e: E): F[Step[F, E, B]] = F.map(self.feedEl(e))(_.map(f))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = F.map(self.feedChunk(h1, h2, t))(_.map(f))
      final def end: F[Done.Ended[F, E, B]] = F.map(self.end)(_.endedMap(f))
    }
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new Effectful[F, E2, A] {
      final def feedEl(e: E2): F[Step[F, E2, A]] = F.map(self.feedEl(f(e)))(_.contramap(f))
      final def feedChunk(h1: E2, h2: E2, t: Vector[E2]): F[Step[F, E2, A]] =
        F.map(self.feedChunk(f(h1), f(h2), t.map(f)))(_.contramap(f))
      final def end: F[Done.Ended[F, E2, A]] = F.map(self.end)(_.endedContramap(f))
    }
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Effectful[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = M.flatMap(self.feedEl(e))(_.bind(f))
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] =
          M.flatMap(self.feedChunk(h1, h2, t))(_.bind(f))
        final def end: F[Done.Ended[F, E, B]] = M.flatMap(self.end)(_.endedBind(f))
      }
    )
  }

  abstract class EffectfulFolder[F[_]: Applicative, E, A]
    extends Effectful[F, E, A] with Input.Folder[E, F[Step[F, E, A]]] {
    final def feedEl(e: E): F[Step[F, E, A]] = onEl(e)
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = onChunk(h1, h2, t)
  }

  abstract class PureFolder[F[_], E, A](implicit F: Applicative[F])
    extends Cont[F, E, A] with Input.Folder[E, Step[F, E, A]]  { self =>
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(onEl(e))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, A]] = F.pure(onChunk(h1, h2, t))

    final def map[B](f: A => B): Step[F, E, B] = new PureFolder[F, E, B] {
      final def onEl(e: E): Step[F, E, B] = self.onEl(e).map(f)
      final def onChunk(h1: E, h2: E, t: Vector[E]): Step[F, E, B] = self.onChunk(h1, h2, t).map(f)
      final def end: F[Done.Ended[F, E, B]] = F.map(self.end)(_.endedMap(f))
    }
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new PureFolder[F, E2, A] {
      final def onEl(e: E2): Step[F, E2, A] = self.onEl(f(e)).contramap(f)
      final def onChunk(h1: E2, h2: E2, t: Vector[E2]): Step[F, E2, A] =
        self.onChunk(f(h1), f(h2), t.map(f)).contramap(f)
      final def end: F[Done.Ended[F, E2, A]] = F.map(self.end)(_.endedContramap(f))
    }
    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Effectful[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = self.onEl(e).bind(f)
        final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = self.onChunk(h1, h2, t).bind(f)
        final def end: F[Done.Ended[F, E, B]] = M.flatMap(self.end)(_.endedBind(f))
      }
    )
  }
}
