package io.iteratee.internal

import cats.{ Applicative, Monad }
import cats.data.NonEmptyVector
import cats.arrow.FunctionK

private[internal] abstract class BaseCont[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] { self =>
  final def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z =
    ifCont { nev =>
      if (nev.length == 1) feedEl(nev.head) else feedChunk(nev.head, nev.getUnsafe(1), nev.tail.drop(1))
    }
  final def isDone: Boolean = false

  final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = new EffectfulCont[G, E, A] {
    final def feedEl(e: E): G[Step[G, E, A]] = f(F.map(self.feedEl(e))(_.mapI(f)))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): G[Step[G, E, A]] = f(F.map(self.feedChunk(h1, h2, t))(_.mapI(f)))
    final def run: G[A] = f(self.run)
  }
  final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = other match {
    case Done(otherValue, _) => map((_, otherValue))
    case step => new EffectfulCont[F, E, (A, B)] {
      final def feedEl(e: E): F[Step[F, E, (A, B)]] = F.map2(self.feedEl(e), step.feedEl(e))(_.zip(_))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, (A, B)]] =
        F.map2(self.feedChunk(h1, h2, t), step.feedChunk(h1, h2, t))(_.zip(_))
      final def run: F[(A, B)] = F.product(self.run, step.run)
    }
  }
}

private[internal] abstract class EffectfulCont[F[_], E, A](implicit F: Applicative[F])
  extends BaseCont[F, E, A] { self =>
  final def map[B](f: A => B): Step[F, E, B] = new EffectfulCont[F, E, B] {
    final def feedEl(e: E): F[Step[F, E, B]] = F.map(self.feedEl(e))(_.map(f))
    final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] = F.map(self.feedChunk(h1, h2, t))(_.map(f))
    final def run: F[B] = F.map(self.run)(f)
  }
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = new EffectfulCont[F, E2, A] {
    final def feedEl(e: E2): F[Step[F, E2, A]] = F.map(self.feedEl(f(e)))(_.contramap(f))
    final def feedChunk(h1: E2, h2: E2, t: Vector[E2]): F[Step[F, E2, A]] =
      F.map(self.feedChunk(f(h1), f(h2), t.map(f)))(_.contramap(f))
    final def run: F[A] = self.run
  }
  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
    new EffectfulCont[F, E, B] {
      final def feedEl(e: E): F[Step[F, E, B]] = M.flatMap(self.feedEl(e))(_.bind(f))
      final def feedChunk(h1: E, h2: E, t: Vector[E]): F[Step[F, E, B]] =
        M.flatMap(self.feedChunk(h1, h2, t))(_.bind(f))
      final def run: F[B] = M.flatMap(self.run)(a => M.flatMap(f(a))(_.run))
    }
  )
}
