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
    final def feed(chunk: NonEmptyVector[E]): G[Step[G, E, A]] = f(F.map(self.feed(chunk))(_.mapI(f)))
    final def run: G[A] = f(self.run)
  }
  final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = other match {
    case Done(otherValue, _) => map((_, otherValue))
    case step => new EffectfulCont[F, E, (A, B)] {
      final def feed(e: NonEmptyVector[E]): F[Step[F, E, (A, B)]] = F.map2(self.feed(chunk), step.feed(chunk))(_.zip(_))
      final def run: F[(A, B)] = F.product(self.run, step.run)
    }
  }
}

private[internal] abstract class EffectfulCont[F[_], E, A](implicit F: Applicative[F])
  extends BaseCont[F, E, A] { self =>
  final def map[B](f: A => B): Step[F, E, B] = new EffectfulCont[F, E, B] {
    final def feed(e: NonEmptyVector[E]): F[Step[F, E, B]] = F.map(self.feed(chunk))(_.map(f))
    final def run: F[B] = F.map(self.run)(f)
  }
  final def contramap[E2](f: E2 => E): Step[F, E2, A] = new EffectfulCont[F, E2, A] {
    final def feed(e: NonEmptyVector[E]): F[Step[F, E2, A]] = F.map(self.feed(chunk.map(f)))(_.contramap(f))
    final def run: F[A] = self.run
  }
  final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
    new EffectfulCont[F, E, B] {
      final def feed(e: NonEmptyVector[E]): F[Step[F, E, B]] = M.flatMap(self.feed(chunk))(_.bind(f))
      final def run: F[B] = M.flatMap(self.run)(a => M.flatMap(f(a))(_.run))
    }
  )
}
