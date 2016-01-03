package io.iteratee

import algebra.{ Monoid, Order, Semigroup }
import cats.{ Applicative, FlatMap, Id, Monad, MonadError, MonoidK }
import io.iteratee.internal.{ Input, Step, diverge }

abstract class Enumerator[F[_], E] extends Serializable { self =>
  def apply[A](s: Step[F, E, A]): F[Step[F, E, A]]

  final def mapE[I](enumeratee: Enumeratee[F, E, I])(implicit M: Monad[F]): Enumerator[F, I] =
    enumeratee.wrap(this)

  final def runStep[A](s: Step[F, E, A])(implicit F: Monad[F]): F[A] =
    F.map(F.flatMap(this(s))(Enumerator.enumEnd[F, E].apply))(_.unsafeValue)

  final def run[A](iteratee: Iteratee[F, E, A])(implicit F: Monad[F]): F[A] =
    F.flatMap(iteratee.state)(runStep)

  final def map[B](f: E => B)(implicit F: Monad[F]): Enumerator[F, B] = mapE(Enumeratee.map(f))

  final def flatMap[B](f: E => Enumerator[F, B])(implicit F: Monad[F]): Enumerator[F, B] =
    mapE(Enumeratee.flatMap(f))

  final def prepend(e: E)(implicit F: Monad[F]): Enumerator[F, E] = {
    new Enumerator[F, E] {
      def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(
        if (step.isDone) F.pure(step) else step.feed(Input.el(e))
      )(self(_))
    }
  }

  final def append(e2: Enumerator[F, E])(implicit F: FlatMap[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(self(s))(e2(_))
    }

  final def flatten[B](implicit M: Monad[F], ev: E =:= F[B]): Enumerator[F, B] =
    flatMap(e => Enumerator.liftM(ev(e)))

  final def bindM[G[_], B](f: E => G[Enumerator[F, B]])(implicit
    F: Monad[F],
    G: Monad[G]
  ): F[G[Enumerator[F, B]]] = {
    val iteratee = Iteratee.fold[F, G[Enumerator[F, B]], G[Enumerator[F, B]]](
      G.pure(Enumerator.empty)
    ) {
      case (acc, concat) => G.flatMap(acc)(en =>
        G.map(concat)(append => Semigroup[Enumerator[F, B]].combine(en, append))
      )
    }

    map(f).run(iteratee)
  }

  final def collect[B](pf: PartialFunction[E, B])(implicit F: Monad[F]): Enumerator[F, B] =
    mapE(Enumeratee.collect(pf))

  final def filter(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, E] =
    mapE(Enumeratee.filter(p))

  final def sequenceI[I](iteratee: Iteratee[F, E, I])(implicit F: Monad[F]): Enumerator[F, I] =
    mapE(Enumeratee.sequenceI(iteratee))

  final def ensure[T](action: F[Unit])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(
        F.handleErrorWith(self(s))(e => F.flatMap(action)(_ => F.raiseError(e)))
      )(result => F.map(action)(_ => result))
    }

  final def uniq(implicit F: Monad[F], E: Order[E]): Enumerator[F, E] = mapE(Enumeratee.uniq)

  final def zipWithIndex(implicit F: Monad[F]): Enumerator[F, (E, Long)] =
    mapE(Enumeratee.zipWithIndex)

  final def grouped(n: Int)(implicit F: Monad[F]): Enumerator[F, Vector[E]] =
    mapE(Enumeratee.grouped(n))

  final def splitOn(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, Vector[E]] =
    mapE(Enumeratee.splitOn(p))

  final def drain(implicit F: Monad[F]): F[Vector[E]] = run(Iteratee.drain)

  final def drainTo[C[_]: Applicative: MonoidK](implicit F: Monad[F]): F[C[E]] =
    run(Iteratee.drainTo)

  final def reduced[B](b: B)(f: (B, E) => B)(implicit F: Monad[F]): Enumerator[F, B] =
    new Enumerator[F, B] {
      final def apply[A](step: Step[F, B, A]): F[Step[F, B, A]] = {
        def check(next: Step[F, E, B]): F[Step[F, B, A]] =
          if (next.isDone) step.feed(Input.el(next.unsafeValue)) else
            F.flatMap(next.onEnd) { next2 =>
              if (next2.isDone) step.feed(Input.el(next2.unsafeValue)) else diverge
            }

        F.flatMap(self(Step.fold[F, E, B](b)(f)))(check(_))
      }
    }

  final def cross[E2](e2: Enumerator[F, E2])(implicit M: Monad[F]): Enumerator[F, (E, E2)] =
    mapE(Enumeratee.cross(e2))
}

final object Enumerator extends EnumeratorInstances {
  private[this] final val defaultChunkSize: Int = 1024

  /**
   * Lift an effectful value into an enumerator.
   */
  final def liftM[F[_], E](fa: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
        F.flatMap(fa)(e => s.feed(Input.el(e)))
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
   * An enumerator that ends the stream.
   */
  final def enumEnd[F[_]: Applicative, E]: Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.onEnd.asInstanceOf[F[Step[F, E, A]]]
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
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feed(Input.el(e))
  }

  private[this] abstract class ChunkedIteratorEnumerator[F[_], E](implicit F: Monad[F])
    extends Enumerator[F, E] {
    def chunks: Iterator[Vector[E]]

    private[this] final def go[A](it: Iterator[Vector[E]], step: Step[F, E, A]): F[Step[F, E, A]] =
      if (it.isEmpty || step.isDone) F.pure(step) else {
        val next = it.next()

        F.flatMap(step.feed(Input.fromVectorUnsafe(next)))(go(it, _))
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
        case h :: t => s.feed(Input.fromPair(h, t.toVector))
      }
    }

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[F[_], E](xs: Vector[E])(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
        if (xs.isEmpty) F.pure(s) else s.feed(Input.fromVectorUnsafe(xs))
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
      if (limit > pos) F.flatMap(s.feed(Input.el(xs(pos))))(loop(pos + 1)) else F.pure(s)

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] = loop(math.max(min, 0))(step)
  }

  /**
   * An enumerator that repeats the given value indefinitely.
   */
  final def repeat[F[_], E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] { self =>
    final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] =
      if (step.isDone) F.pure(step) else F.flatMap(step.feed(Input.el(e)))(apply)
  }

  /**
   * An enumerator that iteratively performs an operation and returns the
   * results.
   */
  final def iterate[F[_], E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      private[this] def loop[A](step: Step[F, E, A], last: E): F[Step[F, E, A]] =
        if (step.isDone) F.pure(step) else F.flatMap(step.feed(Input.el(last)))(loop(_, f(last)))

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
          F.flatMap(step.feed(Input.el(last)))(next => F.flatMap(f(last))(loop(next, _)))

      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, init)
    }
}
