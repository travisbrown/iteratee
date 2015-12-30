package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Comonad, FlatMap, Functor, Id, Monad, MonadError, MonoidK, Show }
import cats.arrow.NaturalTransformation
import io.iteratee.internal.{ Input, Step }

/**
 * An iteratee processes a stream of elements of type `E` and may produce a value of type `F[A]`.
 *
 * @see [[io.iteratee.internal.Step]]
 *
 * @tparam F A type constructor representing a context for effects
 * @tparam E The type of the input data
 * @tparam A The type of the calculated result
 */
sealed class Iteratee[F[_], E, A] private[iteratee] (final val state: F[Step[F, E, A]])
  extends Serializable { self =>

  /**
   * Collapse this iteratee to an effectful value using the provided functions.
   */
  final def foldWith[Z](folder: Step.Folder[F, E, A, Z])(implicit F: Functor[F]): F[Z] =
    F.map(state)(_.foldWith(folder))

  /**
   * Run this iteratee and close the stream so that it must produce an effectful value.
   *
   * @note A well-behaved iteratee will always be in the completed state after processing an
   *       end-of-stream [[io.iteratee.internal.Input]] value.
   */
  final def run(implicit F: Monad[F]): F[A] = runWith(Enumerator.enumEnd)

  final def flatMap[B](f: A => Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.flatMap(state)(_.intoF(a => f(a).state)))

  final def map[B](f: A => B)(implicit F: Functor[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.map(state)(_.map(f)))

  final def contramap[E2](f: E2 => E)(implicit F: Monad[F]): Iteratee[F, E2, A] = {
    def next(s: Step[F, E, A]): F[Step[F, E2, A]] = s.foldWith(
      new Step.Folder[F, E, A, F[Step[F, E2, A]]] {
        def onCont(k: Input[E] => F[Step[F, E, A]]): F[Step[F, E2, A]] =
          F.pure(Step.cont((in: Input[E2]) => F.flatMap(k(in.map(f)))(next)))
        def onDone(value: A, remainder: Input[E]): F[Step[F, E2, A]] =
          F.pure(Step.done(value, if (remainder.isEnd) Input.end else Input.empty))
      }
    )

    Iteratee.iteratee(F.flatMap(state)(next))
  }

  /**
   * Feed an enumerator to this iteratee and run it to return an effectful value.
   */
  final def runWith(enumerator: Enumerator[F, E])(implicit F: Monad[F]): F[A] = enumerator.run(self)

  /**
   * Create a new iteratee that first processes values with the given enumeratee.
   */
  final def through[O](enumeratee: Enumeratee[F, O, E])(implicit F: Monad[F]): Iteratee[F, O, A] =
    Iteratee.joinI(Iteratee.iteratee(F.flatMap(state)(enumeratee(_))))

  final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit
    F: Functor[F]
  ): Iteratee[G, E, A] = {
    def transform: Step[F, E, A] => Step[G, E, A] = _.foldWith(
      new Step.Folder[F, E, A, Step[G, E, A]] {
        def onCont(k: Input[E] => F[Step[F, E, A]]): Step[G, E, A] =
          Step.cont(in => loop(k(in)))
        def onDone(value: A, remainder: Input[E]): Step[G, E, A] = Step.done(value, remainder)
      }
    )

    def loop: F[Step[F, E, A]] => G[Step[G, E, A]] =
      i => f(F.map(i)(transform))

    Iteratee.iteratee(loop(state))
  }

  final def up[G[_]](implicit G: Applicative[G], F: Comonad[F]): Iteratee[G, E, A] = mapI(
    new NaturalTransformation[F, G] {
      def apply[A](a: F[A]): G[A] = G.pure(F.extract(a))
    }
  )

  /**
   * Feeds input elements to this iteratee until it is done, feeds the produced value to the 
   * inner iteratee. Then this iteratee will start over, looping until the inner iteratee is done.
   */
  final def sequenceI(implicit m: Monad[F]): Enumeratee[F, E, A] = Enumeratee.sequenceI(this)

  final def zip[B](other: Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, (A, B)] =
    Iteratee.iteratee(
      F.flatMap(F.product(self.state, other.state)) {
        case (sA, sB) => Step.zip(sA, sB)
      }
    )

  final def handleErrorWith[T](f: T => Iteratee[F, E, A])(implicit
    F: MonadError[F, T]
  ): Iteratee[F, E, A] = Iteratee.iteratee(F.handleErrorWith(state)(e => f(e).state))
}

object Iteratee extends IterateeInstances with CollectionIteratees {
  final def iteratee[F[_], E, A](s: F[Step[F, E, A]]): Iteratee[F, E, A] = new Iteratee[F, E, A](s)

  final def fromStep[F[_], E, A](s: Step[F, E, A])(implicit F: Applicative[F]): Iteratee[F, E, A] =
    iteratee(F.pure(s))

  /**
   * Lift an effectful value into an iteratee.
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    iteratee(Step.liftM(fa))


  final def fail[F[_], T, E, A](e: T)(implicit F: MonadError[F, T]): Iteratee[F, E, A] =
    Iteratee.liftM(F.raiseError[A](e))

  final def cont[F[_]: Applicative, E, A](k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
    fromStep(Step.cont(in => k(in).state))

  final def done[F[_]: Applicative, E, A](d: A, r: Input[E]): Iteratee[F, E, A] =
    fromStep(Step.done(d, r))

  final def identity[F[_]: Applicative, E]: Iteratee[F, E, Unit] = done[F, E, Unit]((), Input.empty)

  final def joinI[F[_], E, I, B](it: Iteratee[F, E, Step[F, I, B]])(implicit
    F: Monad[F]
  ): Iteratee[F, E, B] = iteratee(F.flatMap(it.state)(Step.joinI(_)))
}
