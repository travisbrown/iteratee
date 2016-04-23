package io.iteratee

import algebra.Semigroup
import cats.{ Applicative, FlatMap, Monad, MonadError }
import io.iteratee.internal.Step

abstract class Enumerator[F[_], E] extends Serializable { self =>
  def apply[A](s: Step[F, E, A]): F[Step[F, E, A]]

  final def mapE[I](enumeratee: Enumeratee[F, E, I])(implicit M: Monad[F]): Enumerator[F, I] =
    enumeratee.wrap(this)

  final def runStep[A](s: Step[F, E, A])(implicit F: Monad[F]): F[A] = F.flatMap(this(s))(_.run)

  final def run[A](iteratee: Iteratee[F, E, A])(implicit F: Monad[F]): F[A] = F.flatMap(iteratee.state)(runStep)

  final def map[B](f: E => B)(implicit F: Monad[F]): Enumerator[F, B] = mapE(Enumeratee.map(f))

  @deprecated("Use flatMapM", "0.4.1")
  final def flatMapF[B](f: E => F[B])(implicit F: Monad[F]): Enumerator[F, B] = flatMapM(f)

  final def flatMapM[B](f: E => F[B])(implicit F: Monad[F]): Enumerator[F, B] = mapE(Enumeratee.flatMapM(f))

  final def flatMap[B](f: E => Enumerator[F, B])(implicit F: Monad[F]): Enumerator[F, B] = mapE(Enumeratee.flatMap(f))

  final def prepend(e: E)(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] =
      if (step.isDone) self(step) else F.flatMap(step.feedEl(e))(self(_))
  }

  final def append(e2: Enumerator[F, E])(implicit F: FlatMap[F]): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(self(s))(e2(_))
  }

  final def flatten[B](implicit M: Monad[F], ev: E =:= F[B]): Enumerator[F, B] = flatMap(e => Enumerator.liftM(ev(e)))

  final def bindM[G[_], B](f: E => G[Enumerator[F, B]])(implicit F: Monad[F], G: Monad[G]): F[G[Enumerator[F, B]]] =
    map(f).run(
      Iteratee.fold[F, G[Enumerator[F, B]], G[Enumerator[F, B]]](G.pure(Enumerator.empty)) {
        case (acc, concat) => G.flatMap(acc)(en =>
          G.map(concat)(append => Semigroup[Enumerator[F, B]].combine(en, append))
        )
      }
    )

  final def reduced[B](b: B)(f: (B, E) => B)(implicit F: Monad[F]): Enumerator[F, B] = new Enumerator[F, B] {
    final def apply[A](step: Step[F, B, A]): F[Step[F, B, A]] =
      F.flatMap(self(Step.fold[F, E, B](b)(f)))(next => F.flatMap(next.run)(step.feedEl))
  }

  final def reducedM[B](b: B)(f: (B, E) => F[B])(implicit F: Monad[F]): Enumerator[F, B] = new Enumerator[F, B] {
    final def apply[A](step: Step[F, B, A]): F[Step[F, B, A]] =
      F.flatMap(self(Step.foldM[F, E, B](b)(f)))(next => F.flatMap(next.run)(step.feedEl))
  }

  final def toVector(implicit F: Monad[F]): F[Vector[E]] = run(Iteratee.consume)

  final def ensure[T](action: F[Unit])(implicit F: MonadError[F, T]): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(
      F.handleErrorWith(self(s))(e => F.flatMap(action)(_ => F.raiseError(e)))
    )(result => F.map(action)(_ => result))
  }

  final def handleErrorWith[T](f: T => Enumerator[F, E])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
        F.handleErrorWith(self(s))(t => f(t)(s))
    }
}

final object Enumerator extends EnumeratorInstances {
  private[this] final val defaultChunkSize: Int = 1024

  /**
   * Lift an effectful value into an enumerator.
   */
  final def liftM[F[_], E](fa: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(fa)(s.feedEl)
    }

  /**
   * Create a failed enumerator with the given error.
   */
  final def fail[F[_], T, E](e: T)(implicit F: MonadError[F, T]): Enumerator[F, E] =
    Enumerator.liftM(F.raiseError[E](e))

  /**
   * An empty enumerator.
   */
  final def empty[F[_], E](implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.pure(s)
    }

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  final def perform[F[_], E, B](f: F[B])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(f)(_ => F.pure(s))
    }

  /**
   * An enumerator that produces a single value.
   */
  final def enumOne[F[_]: Applicative, E](e: E): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feedEl(e)
  }

  private[this] abstract class ChunkedIteratorEnumerator[F[_], E](implicit F: Monad[F])
    extends Enumerator[F, E] {
    def chunks: Iterator[Vector[E]]

    private[this] final def go[A](it: Iterator[Vector[E]], step: Step[F, E, A]): F[Step[F, E, A]] =
      if (it.isEmpty || step.isDone) F.pure(step) else {
        it.next() match {
          case Vector(e) => F.flatMap(step.feedEl(e))(go(it, _))
          case h1 +: h2 +: t => F.flatMap(step.feedChunk(h1, h2, t))(go(it, _))
        }
      }

    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = go(chunks, s)
  }

  /**
   * An enumerator that produces values from a stream.
   */
  final def enumStream[F[_]: Monad, E](xs: Stream[E], chunkSize: Int = defaultChunkSize): Enumerator[F, E] =
    new ChunkedIteratorEnumerator[F, E] {
      final def chunks: Iterator[Vector[E]] = xs.grouped(chunkSize).map(_.toVector)
    }

  /**
   * An enumerator that produces values from a list.
   */
  final def enumList[F[_], E](xs: List[E])(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = xs match {
        case Nil => F.pure(s)
        case e :: Nil => s.feedEl(e)
        case h1 :: h2 :: t => s.feedChunk(h1, h2, t.toVector)
      }
    }

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[F[_], E](xs: Vector[E])(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
        xs match {
          case Vector() => F.pure(s)
          case Vector(e) => s.feedEl(e)
          case h1 +: h2 +: t => s.feedChunk(h1, h2, t)
        }
    }

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  final def enumIndexedSeq[F[_], E](
    xs: IndexedSeq[E],
    min: Int = 0,
    max: Int = Int.MaxValue
  )(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] {
    private[this] final val limit = math.min(xs.length, max)

    private[this] final def loop[A](pos: Int)(s: Step[F, E, A]): F[Step[F, E, A]] =
      if (limit > pos) F.flatMap(s.feedEl(xs(pos)))(loop(pos + 1)) else F.pure(s)

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] = loop(math.max(min, 0))(step)
  }

  /**
   * An enumerator that repeats the given value indefinitely.
   */
  final def repeat[F[_], E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] { self =>
    final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] =
      if (step.isDone) F.pure(step) else F.flatMap(step.feedEl(e))(apply)
  }

  /**
   * An enumerator that iteratively performs an operation and returns the
   * results.
   */
  final def iterate[F[_], E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      private[this] def loop[A](step: Step[F, E, A], last: E): F[Step[F, E, A]] =
        if (step.isDone) F.pure(step) else F.flatMap(step.feedEl(last))(loop(_, f(last)))

      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, init)
    }

  /**
   * An enumerator that iteratively performs an effectful operation and returns
   * the results.
   */
  final def iterateM[F[_], E](init: E)(f: E => F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      private[this] def loop[A](step: Step[F, E, A], last: E): F[Step[F, E, A]] =
        if (step.isDone) F.pure(step) else
          F.flatMap(step.feedEl(last))(next => F.flatMap(f(last))(loop(next, _)))

      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, init)
    }

  /**
   * An enumerator that iteratively performs an operation until `None` is
   * generated and returns the results.
   */
  final def iterateUntil[F[_], E](init: E)(f: E => Option[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      private[this] def loop[A](step: Step[F, E, A], last: Option[E]): F[Step[F, E, A]] =
        last match {
          case Some(last) if !step.isDone => F.flatMap(step.feedEl(last))(loop(_, f(last)))
          case _ => F.pure(step)
        }
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, Some(init))
    }

  /**
   * An enumerator that iteratively performs an effectful operation until `None`
   * is generated and returns the results.
   */
  final def iterateUntilM[F[_], E](init: E)(f: E => F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      private[this] def loop[A](step: Step[F, E, A], last: Option[E]): F[Step[F, E, A]] =
        last match {
          case Some(last) if !step.isDone => F.flatMap(step.feedEl(last))(next => F.flatMap(f(last))(loop(next, _)))
          case _ => F.pure(step)
        }
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, Some(init))
    }

  /**
   * An enumerator that returns the result of an effectful operation until
   * `None` is generated.
   */
  final def generateM[F[_], E](f: F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
      if (s.isDone) F.pure(s) else F.flatMap(f) {
        case None => F.pure(s)
        case Some(e) => F.flatMap(s.feedEl(e))(apply)
      }
  }
}
