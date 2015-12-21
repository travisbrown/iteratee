package io.iteratee

import algebra.{ Monoid, Order, Semigroup }
import cats.{ Applicative, FlatMap, Functor, Id, Monad, MonadError, MonoidK }

abstract class Enumerator[F[_], E] extends Serializable { self =>
  def apply[A](s: Step[F, E, A]): Iteratee[F, E, A]

  final def mapE[I](enumeratee: Enumeratee[F, E, I])(implicit M: Monad[F]): Enumerator[F, I] =
    enumeratee.wrap(self)

  final def fold[A](iteratee: Iteratee[F, E, A])(implicit F: Monad[F]): F[A] =
    iteratee.process(self)

  final def map[B](f: E => B)(implicit F: Monad[F]): Enumerator[F, B] = mapE(Enumeratee.map(f))

  final def prepend(e: E)(implicit F: Monad[F]): Enumerator[F, E] = {
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] =
        s.foldWith(
          new StepFolder[F, E, A, Iteratee[F, E, A]] {
            def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = k(Input.el(e))
            def onDone(value: A, remaining: Input[E]): Iteratee[F, E, A] = s.pointI
          }
        ).feed(self)
    }
  }

  final def append(e2: Enumerator[F, E])(implicit F: FlatMap[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](step: Step[F, E, A]): Iteratee[F, E, A] = self(step).feed(e2)
    }

  final def flatMap[B](f: E => Enumerator[F, B])(implicit F: Monad[F]) =
    mapE(Enumeratee.flatMap(f))

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

    map(f).fold(iteratee)
  }

  final def collect[B](pf: PartialFunction[E, B])(implicit F: Monad[F]): Enumerator[F, B] =
    mapE(Enumeratee.collect(pf))

  final def filter(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, E] =
    mapE(Enumeratee.filter(p))

  final def sequenceI[I](iteratee: Iteratee[F, E, I])(implicit F: Monad[F]): Enumerator[F, I] =
    mapE(Enumeratee.sequenceI(iteratee))

  final def ensure[T](action: F[Unit])(implicit F: MonadError[F, T]): Enumerator[F, E] =
    new Enumerator[F, E] {
      final def apply[A](step: Step[F, E, A]): Iteratee[F, E, A] =
        Iteratee.iteratee(
          F.flatMap(
            F.handleErrorWith(
              self(step).step)(e => F.flatMap(action)(_ => F.raiseError(e))
            )
          )(result => F.map(action)(_ => result))
        )
    }

  final def uniq(implicit F: Monad[F], E: Order[E]): Enumerator[F, E] = mapE(Enumeratee.uniq)

  final def zipWithIndex(implicit F: Monad[F]): Enumerator[F, (E, Long)] =
    mapE(Enumeratee.zipWithIndex)

  final def grouped(n: Int)(implicit F: Monad[F]): Enumerator[F, Vector[E]] =
    mapE(Enumeratee.grouped(n))

  final def splitOn(p: E => Boolean)(implicit F: Monad[F]): Enumerator[F, Vector[E]] =
    mapE(Enumeratee.splitOn(p))

  final def drain(implicit F: Monad[F]): F[Vector[E]] = fold(Iteratee.consume)

  final def drainTo[M[_]](implicit M: Monad[F], P: MonoidK[M], Z: Applicative[M]): F[M[E]] =
    fold(Iteratee.consumeIn)

  final def reduced[B](b: B)(f: (B, E) => B)(implicit M: Monad[F]): Enumerator[F, B] =
    new Enumerator[F, B] {
      final def apply[A](step: Step[F, B, A]): Iteratee[F, B, A] = {
        def check(s: Step[F, E, B]): Iteratee[F, B, A] = s.foldWith(
          new StepFolder[F, E, B, Iteratee[F, B, A]] {
            def onCont(k: Input[E] => Iteratee[F, E, B]): Iteratee[F, B, A] = k(Input.end).advance {
              s => s.foldWith(
                new StepFolder[F, E, B, Iteratee[F, B, A]] {
                  def onCont(k: Input[E] => Iteratee[F, E, B]): Iteratee[F, B, A] = Iteratee.diverge
                  def onDone(value: B, remainder: Input[E]): Iteratee[F, B, A] = check(s)
                }
              )
            }
            def onDone(value: B, remainder: Input[E]): Iteratee[F, B, A] = step.foldWith(
              new MapContStepFolder[F, B, A](step) {
                def onCont(k: Input[B] => Iteratee[F, B, A]): Iteratee[F, B, A] = k(Input.el(value))
              }
            )
          }
        )

        Iteratee.iteratee(
          M.flatMap(Iteratee.fold[F, E, B](b)(f).feed(self).step)(check(_).step)
        )
      }
    }
    
  final def cross[E2](e2: Enumerator[F, E2])(implicit M: Monad[F]): Enumerator[F, (E, E2)] =
    mapE(Enumeratee.cross(e2))
}

private[iteratee] trait EnumeratorInstances {
  implicit final def EnumeratorMonoid[F[_]: Monad, E]: Monoid[Enumerator[F, E]] =
    new Monoid[Enumerator[F, E]] {
      def combine(e1: Enumerator[F, E], e2: Enumerator[F, E]): Enumerator[F, E] = e1.append(e2)
      def empty: Enumerator[F, E] = Enumerator.empty
    }

  implicit final def EnumeratorMonad[F[_]](implicit
    M0: Monad[F]
  ): Monad[({ type L[x] = Enumerator[F, x] })#L] =
    new EnumeratorMonad[F] {
      implicit val M: Monad[F] = M0
    }
}

final object Enumerator extends EnumeratorInstances {
  private[this] final val defaultChunkSize: Int = 1024

  final def liftM[F[_], E](fa: F[E])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = Iteratee.iteratee(
        F.flatMap(fa) { e =>
          s.foldWith(
            new MapContStepFolder[F, E, A](s) {
              def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = k(Input.el(e))
            }
          ).step
        }
      )
    }

  final def fail[F[_], T, E](e: T)(implicit F: MonadError[F, T]): Enumerator[F, E] =
    Enumerator.liftM(F.raiseError[E](e))

  final def empty[F[_]: Applicative, E]: Enumerator[F, E] =
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = s.pointI
    }

  /** 
   * An enumerator that is at EOF.
   */
  final def enumEnd[F[_]: Applicative, E]: Enumerator[F, E] =
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = s.foldWith(
        new MapContStepFolder[F, E, A](s) {
          def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = k(Input.end)
        }
      )
    }

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  final def perform[F[_], E, B](f: F[B])(implicit F: Monad[F]): Enumerator[F, E] =
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] =
        Iteratee.iteratee(F.flatMap(f)(_ => F.pure(s)))
    }

  /**
   * An enumerator that produces a single value.
   */
  final def enumOne[F[_]: Applicative, E](e: E): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = s.foldWith(
      new MapContStepFolder[F, E, A](s) {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = k(Input.el(e))
      }
    )
  }

  private[this] abstract class ChunkedIteratorEnumerator[F[_]: Monad, E] extends Enumerator[F, E] {
    def chunks: Iterator[Vector[E]]

    private[this] final def go[A](it: Iterator[Vector[E]], s: Step[F, E, A]): Iteratee[F, E, A] =
      if (it.isEmpty) s.pointI else s.foldWith(
        new MapContStepFolder[F, E, A](s) {
          def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = {
            val next = it.next()

            k(Input.chunk(next)).advance(go(it, _))
          }
        }
      )

    final def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = go(chunks, s)
  }

  /**
   * An enumerator that produces values from a stream.
   */
  final def enumStream[F[_]: Monad, E](
    xs: Stream[E],
    chunkSize: Int = defaultChunkSize
  ): Enumerator[F, E] = new ChunkedIteratorEnumerator[F, E] {
    def chunks: Iterator[Vector[E]] = xs.grouped(chunkSize).map(_.toVector)
  }

  /**
   * An enumerator that produces values from a list.
   */
  final def enumList[F[_]: Monad, E](xs: List[E]): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] =
      if (xs.isEmpty) s.pointI else s.foldWith(
        new MapContStepFolder[F, E, A](s) {
          def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
            k(Input.chunk(xs.toVector))
        }
      )
  }

  /**
   * An enumerator that produces values from a vector.
   */
  final def enumVector[F[_]: Monad, E](xs: Vector[E]): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] =
      if (xs.isEmpty) s.pointI else s.foldWith(
        new MapContStepFolder[F, E, A](s) {
          def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = k(Input.chunk(xs))
        }
      )
  }

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  final def enumIndexedSeq[F[_]: Monad, E](
    xs: IndexedSeq[E],
    min: Int = 0,
    max: Int = Int.MaxValue
  ): Enumerator[F, E] = new Enumerator[F, E] {
    private val limit = math.min(xs.length, max)

    private[this] def loop[A](pos: Int)(s: Step[F, E, A]): Iteratee[F, E, A] = s.foldWith(
      new MapContStepFolder[F, E, A](s) {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
          if (limit > pos) k(Input.el(xs(pos))).advance(loop(pos + 1)) else s.pointI
      }
    )


    def apply[A](step: Step[F, E, A]): Iteratee[F, E, A] = loop(math.max(min, 0))(step)
  }

  /**
   * An enumerator that repeats a given value indefinitely.
   */
  final def repeat[F[_]: Monad, E](e: E): Enumerator[F, E] = new Enumerator[F, E] {
    def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = s.foldWith(
      new MapContStepFolder[F, E, A](s) {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
          k(Input.el(e)).advance(apply[A])
      }
    )
  }

  /**
   * An enumerator that iteratively performs an operation.
   */
  final def iterate[F[_]: Monad, E](init: E)(f: E => E): Enumerator[F, E] = new Enumerator[F, E] {
    private[this] def loop[A](s: Step[F, E, A], last: E): Iteratee[F, E, A] = s.foldWith(
      new MapContStepFolder[F, E, A](s) {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
          k(Input.el(last)).advance(step => loop(step, f(last)))
      }
    )

    def apply[A](s: Step[F, E, A]): Iteratee[F, E, A] = loop(s, init)
  }
}

private trait EnumeratorFunctor[F[_]] extends Functor[({ type L[x] = Enumerator[F, x] })#L] {
  implicit def M: Monad[F]
  abstract override def map[A, B](fa: Enumerator[F, A])(f: A => B): Enumerator[F, B] = fa.map(f)
}

private trait EnumeratorMonad[F[_]] extends Monad[({ type L[x] = Enumerator[F, x] })#L]
  with EnumeratorFunctor[F] {
  final def flatMap[A, B](fa: Enumerator[F, A])(f: A => Enumerator[F, B]): Enumerator[F, B] =
    fa.flatMap(f)
  final def pure[E](e: E): Enumerator[F, E] = Enumerator.enumOne[F, E](e)
}
