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
        s.mapCont(_(Input.el(e))).feedE(self)
    }
  }

  def flatMap[B](f: E => Enumerator[B, F])(implicit M1: Monad[F]) =
    Enumeratee.flatMap(f).wrap(self)

  def flatten[B](implicit ev: E =:= F[B], M: Monad[F]): Enumerator[B, F] =
    flatMap(e => Enumerator.liftM[F, B](ev(e)))

  def bindM[B, G[_]](f: E => G[Enumerator[B, F]])(implicit F: Monad[F], G: Monad[G]): F[G[Enumerator[B, F]]] = {
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

  def drainTo(implicit F: Monad[F]): F[Vector[E]] =
    Iteratee.consume[E, F].feedE(self).run

  def drainToF[M[_]](implicit M: Monad[F], P: MonoidK[M], Z: Applicative[M]): F[M[E]] =
    Iteratee.consumeIn[E, F, M].feedE(self).run

  def reduced[B](b: B)(f: (B, E) => B)(implicit M: Monad[F]): Enumerator[B, F] =
    new Enumerator[B, F] {
      def apply[A](step: Step[B, F, A]): Iteratee[B, F, A] = {
        def check(s: Step[E, F, B]): Iteratee[B, F, A] = s.foldWith(
          new StepFolder[E, F, B, Iteratee[B, F, A]] {
            def onCont(k: Input[E] => Iteratee[E, F, B]): Iteratee[B, F, A] = k(Input.eof).feed {
              s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s))
            }
            def onDone(value: B, remainder: Input[E]): Iteratee[B, F, A] = step.mapCont(f => f(Input.el(value)))
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
  implicit def EnumeratorMonoid[E, F[_]](implicit F0: Monad[F]): Monoid[Enumerator[E, F]] =
    new EnumeratorMonoid[E, F] {
      implicit def F = F0
    }

  implicit def EnumeratorMonad[F[_]](implicit
    M0: Monad[F]
  ): Monad[({ type L[x] = Enumerator[x, F] })#L] =
    new EnumeratorMonad[F] {
      implicit val M = M0
    }
}

object Enumerator extends EnumeratorInstances {
  def liftM[G[_]: Monad, E](ga: G[E]): Enumerator[E, G] =
    new Enumerator[E, G] {
      def apply[A](s: Step[E, G, A]): Iteratee[E, G, A] =
        Iteratee.iteratee(
          Monad[G].flatMap(ga) { e =>
            s.mapCont(k => k(Input.el(e))).value
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
  def enumEof[E, F[_]: Applicative]: Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.mapCont(_(Input.eof))
    }

  /**
   * An enumerator that forces the evaluation of an effect in the F monad when it is consumed.
   */
  def perform[E, F[_]: Monad, B](f: F[B]): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
        Iteratee.iteratee(Monad[F].flatMap(f) { _ => s.pointI.value })
    }

  def enumOne[E, F[_]: Applicative](e: E): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
        s.mapCont(_(Input.el(e)))
    }

  def enumStream[E, F[_]: Monad](xs: Stream[E]): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = xs match {
        case h #:: t => s.mapCont(_(Input.el(h)).feedE(enumStream[E, F](t)))
        case _       => s.pointI
      }
    }

  def enumList[E, F[_]: Monad](xs: List[E]): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] =
        if (xs.isEmpty) s.pointI else s.mapCont(_(Input.chunk(xs)))
    }

  def enumIndexedSeq[E, F[_]: Monad](a: IndexedSeq[E], min: Int = 0, max: Option[Int] = None): Enumerator[E, F] =
    new Enumerator[E, F] {
      private val limit = max.map(_.min(a.length)).getOrElse(a.length)
      def apply[A](step: Step[E, F, A]): Iteratee[E, F, A] = {
        def loop(pos: Int): Step[E, F, A] => Iteratee[E, F, A] = {
          s => 
            s.mapCont(
              k => if (limit > pos) k(Input.el(a(pos))).feed(loop(pos + 1))
                   else             s.pointI
            )   
        }
        loop(min)(step)
      }
    }

  /**
   * An enumerator that yields the elements of the specified array from index min (inclusive) to max (exclusive)
   */
  def enumArray[E, F[_]: Monad](a: Array[E], min: Int = 0, max: Option[Int] = None): Enumerator[E, F] =
    enumIndexedSeq(a, min, max)

  def repeat[E, F[_]: Monad](e: E): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.mapCont(_(Input.el(e)).feed(apply[A]))
    }

  def iterate[E, F[_]: Monad](f: E => E, e: E): Enumerator[E, F] =
    new Enumerator[E, F] {
      private[this] def loop[A](s: Step[E, F, A], last: E): Iteratee[E, F, A] =
        s.mapCont(_(Input.el(last)).feed(step => loop(step, f(last))))

      def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = loop(s, e)
    }
}

// Instances are mixed in with the [[Iteratee]] object

//
// Type class implementation traits
//

private trait EnumeratorSemigroup[E, F[_]] extends Semigroup[Enumerator[E, F]] {
  implicit def F: FlatMap[F]

  def combine(e1: Enumerator[E, F], e2: Enumerator[E, F]): Enumerator[E, F] =
    new Enumerator[E, F] {
      def apply[A](step: Step[E, F, A]): Iteratee[E, F, A] = e1(step).feedE(e2)
    }
}

private trait EnumeratorMonoid[E, F[_]] extends Monoid[Enumerator[E, F]] with EnumeratorSemigroup[E, F] {
  implicit def F: Monad[F]

  def empty: Enumerator[E, F] = new Enumerator[E, F] {
    def apply[A](s: Step[E, F, A]): Iteratee[E, F, A] = s.pointI
  }
}

private trait EnumeratorFunctor[F[_]] extends Functor[({ type L[x] = Enumerator[x, F] })#L] {
  implicit def M: Monad[F]
  abstract override def map[A, B](fa: Enumerator[A, F])(f: A => B): Enumerator[B, F] = fa.map(f)
}

private trait EnumeratorMonad[F[_]] extends Monad[({ type L[x] = Enumerator[x, F] })#L]
  with EnumeratorFunctor[F] {
  def flatMap[A, B](fa: Enumerator[A, F])(f: A => Enumerator[B, F]) = fa.flatMap(f)
  def pure[E](e: E) = Enumerator.enumOne[E, F](e)
}
