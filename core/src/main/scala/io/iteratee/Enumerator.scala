package io.iteratee

import cats.{ Applicative, FlatMap, Monad, MonadError, Semigroup, Eval }
import cats.kernel.Eq
import io.iteratee.internal.Step
import scala.Predef.=:=
import scala.util.{ Left, Right }

/**
 *
 * @tparam F The effect type constructor
 * @tparam E The type of the enumerated data
 */
abstract class Enumerator[F[_], E] extends Serializable { self =>
  def apply[A](s: Step[F, E, A]): F[Step[F, E, A]]

  final def through[I](enumeratee: Enumeratee[F, E, I])(implicit M: FlatMap[F]): Enumerator[F, I] =
    enumeratee.wrap(this)

  final def intoStep[A](s: Step[F, E, A])(implicit F: FlatMap[F]): F[A] = F.flatMap(this(s))(_.run)

  final def into[A](iteratee: Iteratee[F, E, A])(implicit F: FlatMap[F]): F[A] = F.flatMap(iteratee.state)(intoStep)

  final def map[B](f: E => B)(implicit F: Monad[F]): Enumerator[F, B] = through(Enumeratee.map(f))

  final def flatMapM[B](f: E => F[B])(implicit F: Monad[F]): Enumerator[F, B] = through(Enumeratee.flatMapM(f))

  final def flatMap[B](f: E => Enumerator[F, B])(implicit F: Monad[F]): Enumerator[F, B] =
    through(Enumeratee.flatMap(f))

  final def take(n: Long)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.take[F, E](n))
  final def takeWhile(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.takeWhile[F, E](p))
  final def takeWhileM(p: E => F[Boolean])(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.takeWhileM(p))
  final def drop(n: Long)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.drop[F, E](n))
  final def dropWhile(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.dropWhile[F, E](p))
  final def dropWhileM(p: E => F[Boolean])(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.dropWhileM(p))
  final def scan[A](init: A)(f: (A, E) => A)(implicit F: Monad[F]): Enumerator[F, A] =
    through(Enumeratee.scan[F, E, A](init)(f))
  final def scanM[A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Enumerator[F, A] =
    through(Enumeratee.scanM[F, E, A](init)(f))
  final def collect[B](pf: PartialFunction[E, B])(implicit F: Monad[F]): Enumerator[F, B] =
    through(Enumeratee.collect[F, E, B](pf))
  final def filter(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.filter[F, E](p))
  final def filterM(p: E => F[Boolean])(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.filterM(p))
  final def sequenceI[B](iteratee: Iteratee[F, E, B])(implicit F: Monad[F]): Enumerator[F, B] =
    through(Enumeratee.sequenceI(iteratee))
  final def uniq(implicit F: Monad[F], E: Eq[E]): Enumerator[F, E] = through(Enumeratee.uniq[F, E])
  final def zipWithIndex(implicit F: Monad[F]): Enumerator[F, (E, Long)] = through(Enumeratee.zipWithIndex[F, E])
  final def grouped(n: Int)(implicit F: Monad[F]): Enumerator[F, Vector[E]] = through(Enumeratee.grouped[F, E](n))
  final def splitOn(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, Vector[E]] =
    through(Enumeratee.splitOn[F, E](p))
  final def cross[E2](e2: Enumerator[F, E2])(implicit F: Monad[F]): Enumerator[F, (E, E2)] =
    through(Enumeratee.cross(e2))
  final def intersperse(delim: E)(implicit F: Monad[F]): Enumerator[F, E] = through(Enumeratee.intersperse[F, E](delim))

  final def prepend(e: E)(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] =
      if (step.isDone) self(step) else F.flatMap(step.feedEl(e))(self(_))
  }

  final def append(e2: Enumerator[F, E])(implicit F: FlatMap[F]): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(self(s))(e2(_))
  }

  final def flatten[B](implicit M: Monad[F], ev: E =:= F[B]): Enumerator[F, B] = flatMap(e => Enumerator.liftM(ev(e)))

  final def bindM[G[_], B](f: E => G[Enumerator[F, B]])(implicit F: Monad[F], G: Monad[G]): F[G[Enumerator[F, B]]] =
    map(f).into(
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

  final def toVector(implicit F: Monad[F]): F[Vector[E]] = into(Iteratee.consume)

  final def ensure[T](action: F[Unit])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    ensureEval(Eval.now(action))

  final def ensureEval[T](action: Eval[F[Unit]])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(
        F.handleErrorWith(self(s))(e => F.flatMap(action.value)(_ => F.raiseError(e)))
      )(result => F.map(action.value)(_ => result))
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
   * An enumerator that produces the given values.
   */
  final def enumerate[F[_], E](xs: E*)(implicit F: Applicative[F]): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feed(xs)
  }

  /**
   * Lift an effectful value into an enumerator.
   */
  final def liftM[F[_], E](fa: F[E])(implicit F: FlatMap[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(fa)(s.feedEl)
    }
  /**
   * Lift an effectful value in a [[cats.Eval]] into an enumerator.
   */
  final def liftMEval[F[_], E](fa: Eval[F[E]])(implicit F: FlatMap[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.flatMap(fa.value)(s.feedEl)
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
  final def perform[F[_], E, B](f: F[B])(implicit F: FlatMap[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.map(f)(_ => s)
    }

  /**
   * An enumerator that produces a single value.
   */
  final def enumOne[F[_]: Applicative, E](e: E): Enumerator[F, E] = new Enumerator[F, E] {
    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feedEl(e)
  }

  /**
   * An enumerator that either produces a single value or fails.
   */
  final def enumEither[F[_], T, E](either: Either[T, E])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    either match {
      case Right(a) => enumOne(a)
      case Left(t) => fail(t)
    }

  private[this] abstract class ChunkedIteratorEnumerator[F[_], E](implicit F: Monad[F]) extends Enumerator[F, E] {
    protected[this] def chunks: Iterator[Seq[E]]

    final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM((s, chunks)) {
      case (step, it) => if (it.isEmpty || step.isDone) F.pure(Right(step)) else {
        F.map(step.feed(it.next()))(s => Left((s, it)))
      }
    }
  }

  /**
   * An enumerator that produces values from an iterable collection.
   */
  final def enumIterable[F[_]: Monad, E](xs: Iterable[E], chunkSize: Int = defaultChunkSize): Enumerator[F, E] =
    new ChunkedIteratorEnumerator[F, E] {
      final protected[this] def chunks: Iterator[Seq[E]] = xs.grouped(math.max(chunkSize, 1)).map(_.toSeq)
    }

  /**
   * An enumerator that produces values from a stream.
   */
  final def enumStream[F[_]: Monad, E](xs: Stream[E], chunkSize: Int = defaultChunkSize): Enumerator[F, E] =
    new ChunkedIteratorEnumerator[F, E] {
      final protected[this] def chunks: Iterator[Seq[E]] = xs.grouped(math.max(chunkSize, 1))
    }

  /**
   * An enumerator that produces values from a list.
   */
  final def enumList[F[_], E](xs: List[E])(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feed(xs)
    }

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[F[_], E](xs: Vector[E])(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feed(xs)
    }

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  final def enumIndexedSeq[F[_], E](
    xs: IndexedSeq[E],
    min: Int = 0,
    max: Int = Int.MaxValue
  )(implicit F: Applicative[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = s.feed(xs.slice(min, max))
    }

  /**
   * An enumerator that repeats the given value indefinitely.
   */
  final def repeat[F[_], E](e: E)(implicit F: Monad[F]): Enumerator[F, E] = new Enumerator[F, E] { self =>
    final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM(step) { s =>
      if (s.isDone) F.pure(Right(s)) else F.map(s.feedEl(e))(Left(_))
    }
  }

  /**
   * An enumerator that iteratively performs an operation and returns the
   * results.
   */
  final def iterate[F[_], E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM((s, init)) {
        case (step, last) => if (step.isDone) F.pure(Right(step)) else {
          F.map(step.feedEl(last))(s1 => Left((s1, f(last))))
        }
      }
    }

  /**
   * An enumerator that iteratively performs an effectful operation and returns
   * the results.
   */
  final def iterateM[F[_], E](init: E)(f: E => F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM((s, init)) {
        case (step, last) => if (step.isDone) F.pure(Right(step)) else {
          F.map2(step.feedEl(last), f(last))((s1, next) => Left((s1, next)))
        }
      }
    }

  /**
   * An enumerator that iteratively performs an operation until `None` is
   * generated and returns the results.
   */
  final def iterateUntil[F[_], E](init: E)(f: E => Option[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM((s, Some(init): Option[E])) {
        case (step, Some(last)) if !step.isDone => F.map(step.feedEl(last))(s1 => Left((s1, f(last))))
        case (step, _) => F.pure(Right(step))
      }
    }

  /**
   * An enumerator that iteratively performs an effectful operation until `None`
   * is generated and returns the results.
   */
  final def iterateUntilM[F[_], E](init: E)(f: E => F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM((s, Some(init): Option[E])) {
        case (step, Some(last)) if !step.isDone =>
          F.map2(step.feedEl(last), f(last))((s1, next) => Left((s1, next)))
        case (step, _) => F.pure(Right(step))
      }
    }

  /**
   * An enumerator that returns the result of an effectful operation until
   * `None` is generated.
   */
  final def generateM[F[_], E](f: F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.tailRecM(s) { step =>
        if (step.isDone) F.pure(Right(step)) else F.flatMap(f) {
          case Some(e) => F.map(step.feedEl(e))(Left(_))
          case None => F.pure(Right(step))
        }
      }
    }

  /**
   * Enumerators that rely on `F` to provide stack safety.
   *
   * These implementations will generally be more efficient than the default
   * ones, but will not be stack safe unless recursive monadic binding in `F` is
   * stack safe.
   */
  final object StackUnsafe {
    private[this] abstract class ChunkedIteratorEnumerator[F[_], E](implicit F: Monad[F]) extends Enumerator[F, E] {
      def chunks: Iterator[Vector[E]]

      private[this] final def go[A](it: Iterator[Vector[E]], step: Step[F, E, A]): F[Step[F, E, A]] =
        if (it.isEmpty || step.isDone) F.pure(step) else {
          val next = it.next()

          F.flatMap(step.feed(next))(go(it, _))
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
     * An enumerator that repeats the given value indefinitely.
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    final def repeat[F[_], E](e: E)(implicit F: Monad[F]): Enumerator[F, E] =
      new Enumerator[F, E] {
        final def apply[A](step: Step[F, E, A]): F[Step[F, E, A]] =
          if (step.isDone) F.pure(step) else F.flatMap(step.feedEl(e))(apply)
      }

    /**
     * An enumerator that iteratively performs an operation and returns the
     * results.
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    def iterate[F[_], E](init: E)(f: E => E)(implicit F: Monad[F]): Enumerator[F, E] =
      new Enumerator[F, E] {
        private[this] def loop[A](step: Step[F, E, A], last: E): F[Step[F, E, A]] =
          if (step.isDone) F.pure(step) else F.flatMap(step.feedEl(last))(loop(_, f(last)))

        final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, init)
      }

    /**
     * An enumerator that iteratively performs an effectful operation and returns
     * the results.
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    def iterateM[F[_], E](init: E)(f: E => F[E])(implicit F: Monad[F]): Enumerator[F, E] =
      new Enumerator[F, E] {
        private[this] def loop[A](step: Step[F, E, A], last: E): F[Step[F, E, A]] =
          if (step.isDone) F.pure(step) else F.flatten(F.map2(step.feedEl(last), f(last))(loop))

        final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, init)
      }

    /**
     * An enumerator that iteratively performs an operation until `None` is
     * generated and returns the results.
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    def iterateUntil[F[_], E](init: E)(f: E => Option[E])(implicit F: Monad[F]): Enumerator[F, E] =
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
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    def iterateUntilM[F[_], E](init: E)(f: E => F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
      new Enumerator[F, E] {
        private[this] def loop[A](step: Step[F, E, A], last: Option[E]): F[Step[F, E, A]] =
          last match {
            case Some(last) if !step.isDone => F.flatten(F.map2(step.feedEl(last), f(last))(loop))
            case _ => F.pure(step)
          }
        final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = loop(s, Some(init))
      }

    /**
     * An enumerator that returns the result of an effectful operation until
     * `None` is generated.
     *
     * Note that this implementation will only be stack safe if recursive monadic
     * binding in `F` is stack safe.
     */
    def generateM[F[_], E](f: F[Option[E]])(implicit F: Monad[F]): Enumerator[F, E] =
      new Enumerator[F, E] {
        final def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] =
          if (s.isDone) F.pure(s) else F.flatMap(f) {
            case None => F.pure(s)
            case Some(e) => F.flatMap(s.feedEl(e))(apply)
          }
      }
  }
}
