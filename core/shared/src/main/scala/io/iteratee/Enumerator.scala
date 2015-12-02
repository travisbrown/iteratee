package io.iteratee

import algebra.{ Monoid, Order, Semigroup }
import cats.{ Applicative, FlatMap, Functor, Id, Monad, MonoidK }

abstract class Enumerator[E, F[_]] { self =>
  def apply[A](s: Step[E, F, A]): Iteratee[E, F, A]

  def mapE[I](enumeratee: Enumeratee[E, I, F])(implicit M: Monad[F]): Enumerator[I, F] =
    enumeratee.wrap(self)

  def map[B](f: E => B)(implicit ev: Monad[F]): Enumerator[B, F] =
    Enumeratee.map[E, B, F](f).wrap(self)

  def #::(e: => E)(implicit F: Monad[F]): Enumerator[E, F] = {
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
        s.foldWith(
          new StepFolder[E, F, A, Iteratee[E, F, A]] {
            def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] = k(Input.el(e))
            def onDone(value: A, remaining: Input[E]): Iteratee[E, F, A] = s.pointI
          }
        ).feedE(self)
    }
  }

  def append(e2: Enumerator[E, F])(implicit F: FlatMap[F]): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](step: Step[E, F, A]): Iteratee[E, F, A] = self(step).feedE(e2)
    }

  def flatMap[B](f: E => Enumerator[B, F])(implicit M1: Monad[F]) =
    Enumeratee.flatMap(f).wrap(self)

  def flatten[B](implicit ev: E =:= F[B], M: Monad[F]): Enumerator[B, F] =
    flatMap(e => Enumerator.liftM[F, B](ev(e)))

  def bindM[B, G[_]](f: E => G[Enumerator[B, F]])(implicit
    F: Monad[F],
    G: Monad[G]
  ): F[G[Enumerator[B, F]]] = {
    val iteratee = Iteratee.fold[G[Enumerator[B, F]], F, G[Enumerator[B, F]]](
      G.pure(Enumerator.empty[B, F])
    ) {
      case (acc, concat) => G.flatMap(acc) { en =>
        G.map(concat) { append => Semigroup[Enumerator[B, F]].combine(en, append) }
      }
    }   

    iteratee.feedE(self.map(f)).run
  }

  def collect[B](pf: PartialFunction[E, B])(implicit F: Monad[F]): Enumerator[B, F] =
    Enumeratee.collect[E, B, F](pf).wrap(self)

  def filter(p: E => Boolean)(implicit F: Monad[F]): Enumerator[E, F] =
    Enumeratee.filter[E, F](p).wrap(self)

  def uniq(implicit ord: Order[E], F: Monad[F]): Enumerator[E, F] =
    Enumeratee.uniq[E, F].wrap(self)

  def zipWithIndex(implicit F: Monad[F]): Enumerator[(E, Long), F] =
    Enumeratee.zipWithIndex[E, F].wrap(self)

  def grouped(n: Int)(implicit F: Monad[F]): Enumerator[Vector[E], F] =
    Enumeratee.grouped[E, F](n).wrap(self)

  def splitOn(p: E => Boolean)(implicit F: Monad[F]): Enumerator[Vector[E], F] =
    Enumeratee.splitOn[E, F](p).wrap(self)

  def drain(implicit F: Monad[F]): F[Vector[E]] = Iteratee.consume[E, F].feedE(self).run

  def drainTo[M[_]](implicit M: Monad[F], P: MonoidK[M], Z: Applicative[M]): F[M[E]] =
    Iteratee.consumeIn[E, F, M].feedE(self).run

  def reduced[B](b: B)(f: (B, E) => B)(implicit M: Monad[F]): Enumerator[B, F] =
    new Enumerator[B, F] {
      def apply[A](step: Step[B, F, A]): Iteratee[B, F, A] = {
        def check(s: Step[E, F, B]): Iteratee[B, F, A] = s.foldWith(
          new StepFolder[E, F, B, Iteratee[B, F, A]] {
            def onCont(k: Input[E] => Iteratee[E, F, B]): Iteratee[B, F, A] = k(Input.end).feed {
              s => s.foldWith(
                new StepFolder[E, F, B, Iteratee[B, F, A]] {
                  def onCont(k: Input[E] => Iteratee[E, F, B]): Iteratee[B, F, A] = Iteratee.diverge
                  def onDone(value: B, remainder: Input[E]): Iteratee[B, F, A] = check(s)
                }
              )
            }
            def onDone(value: B, remainder: Input[E]): Iteratee[B, F, A] = step.foldWith(
              new MapContStepFolder[B, F, A](step) {
                def onCont(k: Input[B] => Iteratee[B, F, A]): Iteratee[B, F, A] = k(Input.el(value))
              }
            )
          }
        )

        Iteratee.iteratee(
          M.flatMap(Iteratee.fold[E, F, B](b)(f).feedE(self).value)(check(_).value)
        )
      }
    }
    
  def cross[E2](e2: Enumerator[E2, F])(implicit M: Monad[F]): Enumerator[(E, E2), F] =
    Enumeratee.cross[E, E2, F](e2).wrap(self)
}

trait EnumeratorInstances {
  implicit def EnumeratorMonoid[E, F[_]: Monad]: Monoid[Enumerator[E, F]] =
    new Monoid[Enumerator[E, F]] {
      def combine(e1: Enumerator[E, F], e2: Enumerator[E, F]): Enumerator[E, F] = e1.append(e2)
      def empty: Enumerator[E, F] = Enumerator.empty[E, F]
    }

  implicit def EnumeratorMonad[F[_]](implicit
    M0: Monad[F]
  ): Monad[({ type L[x] = Enumerator[x, F] })#L] =
    new EnumeratorMonad[F] {
      implicit val M: Monad[F] = M0
    }
}

object Enumerator extends EnumeratorInstances {
  def liftM[G[_]: Monad, E](ga: G[E]): Enumerator[E, G] =
    new Enumerator[E, G] {
      def apply[A](s: Step[E, G, A]): Iteratee[E, G, A] =
        new Iteratee(
          Monad[G].flatMap(ga) { e =>
            s.foldWith(
              new MapContStepFolder[E, G, A](s) {
                def onCont(k: Input[E] => Iteratee[E, G, A]): Iteratee[E, G, A] =
                  k(Input.el(e))
              }
            ).value
          }
        )
    }

  def enumerate[E](as: Stream[E]): PureEnumerator[E] = enumStream[E, Id](as)

  def empty[E, F[_]: Applicative]: Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.pointI
    }

  /** 
   * An Enumerator that is at EOF
   */
  def enumEnd[E, F[_]: Applicative]: Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.foldWith(
        new MapContStepFolder[E, F, A](s) {
          def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] = k(Input.end)
        }
      )
    }

  /**
   * An enumerator that forces the evaluation of an effect when it is consumed.
   */
  def perform[E, F[_]: Monad, B](f: F[B]): Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
      new Iteratee(Monad[F].flatMap(f)(_ => s.pointI.value))
  }

  /**
   * An enumerator that produces a single value.
   */
  def enumOne[E, F[_]: Applicative](e: E): Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.foldWith(
      new MapContStepFolder[E, F, A](s) {
        def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] = k(Input.el(e))
      }
    )
  }

  /**
   * An enumerator that produces values from a stream.
   */
  def enumStream[E, F[_]: Monad](xs: Stream[E]): Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = xs match {
      case h #:: t => s.foldWith(
        new MapContStepFolder[E, F, A](s) {
          def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] =
            k(Input.el(h)).feedE(enumStream[E, F](t))
        }
      )
      case _ => s.pointI
    }
  }

  /**
   * An enumerator that produces values from a list.
   */
  def enumList[E, F[_]: Monad](xs: List[E]): Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
      if (xs.isEmpty) s.pointI else s.foldWith(
        new MapContStepFolder[E, F, A](s) {
          def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] = k(Input.chunk(xs))
        }
      )
  }

  /**
   * An enumerator that produces values from a slice of an indexed sequence.
   */
  def enumIndexedSeq[E, F[_]: Monad](
    xs: IndexedSeq[E],
    min: Int = 0,
    max: Int = Int.MaxValue
  ): Enumerator[E, F] = new Enumerator[E, F] {
    private val limit = math.min(xs.length, max)

    private[this] def loop[A](pos: Int)(s: Step[E, F, A]): Iteratee[E, F, A] = s.foldWith(
      new MapContStepFolder[E, F, A](s) {
        def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] =
          if (limit > pos) k(Input.el(xs(pos))).feed(loop(pos + 1)) else s.pointI
      }
    )


    def apply[A](step: Step[E, F, A]): Iteratee[E, F, A] = loop(min)(step)
  }

  /**
   * An enumerator that repeats a given value indefinitely.
   */
  def repeat[E, F[_]: Monad](e: E): Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.foldWith(
      new MapContStepFolder[E, F, A](s) {
        def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] =
          k(Input.el(e)).feed(apply[A])
      }
    )
  }

  /**
   * An enumerator that iteratively performs an operation.
   */
  def iterate[E, F[_]: Monad](f: E => E, e: E): Enumerator[E, F] = new Enumerator[E, F] {
    private[this] def loop[A](s: Step[E, F, A], last: E): Iteratee[E, F, A] = s.foldWith(
      new MapContStepFolder[E, F, A](s) {
        def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] =
          k(Input.el(last)).feed(step => loop(step, f(last)))
      }
    )

    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = loop(s, e)
  }
}

private trait EnumeratorFunctor[F[_]] extends Functor[({ type L[x] = Enumerator[x, F] })#L] {
  implicit def M: Monad[F]
  abstract override def map[A, B](fa: Enumerator[A, F])(f: A => B): Enumerator[B, F] = fa.map(f)
}

private trait EnumeratorMonad[F[_]] extends Monad[({ type L[x] = Enumerator[x, F] })#L]
  with EnumeratorFunctor[F] {
  def flatMap[A, B](fa: Enumerator[A, F])(f: A => Enumerator[B, F]): Enumerator[B, F] =
    fa.flatMap(f)
  def pure[E](e: E): Enumerator[E, F] = Enumerator.enumOne[E, F](e)
}
