package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Comonad, FlatMap, Functor, Id, Monad, MonadError, MonoidK, Show }
import cats.arrow.NaturalTransformation
import io.iteratee.internal.{ Input, Step }

/**
 * An iteratee processes a stream of elements of type `E` and returns a value of
 * type `F[A]`.
 *
 * @see [[io.iteratee.internal.Step]]
 *
 * @tparam F A type constructor representing a context for effects
 * @tparam E The type of the input data
 * @tparam A The type of the calculated result
 */
sealed class Iteratee[F[_], E, A] private[iteratee] (final val state: F[Step[F, E, A]]) extends Serializable { self =>
  /**
   * Run this iteratee and close the stream so that it must produce an effectful
   * value.
   *
   * @note A well-behaved iteratee will always be in the completed state after
   *       processing an [[io.iteratee.internal.Input.end]] value.
   */
  final def run(implicit F: Monad[F]): F[A] = runWith(Enumerator.enumEnd)

  /**
   * Feed an enumerator to this iteratee and run it to return an effectful
   * value.
   */
  final def runWith(enumerator: Enumerator[F, E])(implicit F: Monad[F]): F[A] = enumerator.run(self)

  /**
   * Map a function over the result of this [[Iteratee]].
   */
  final def map[B](f: A => B)(implicit F: Functor[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.map(state)(_.map(f)))

  /**
   * Map a monadic function over the result of this [[Iteratee]].
   */
  final def flatMap[B](f: A => Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.flatMap(state)(_.bindF(a => f(a).state)))

  /**
   * Transform the inputs to this [[Iteratee]].
   */
  final def contramap[E2](f: E2 => E)(implicit F: Monad[F]): Iteratee[F, E2, A] = {
    def loop(s: Step[F, E, A]): F[Step[F, E2, A]] = s.foldWith(
      new Step.Folder[F, E, A, F[Step[F, E2, A]]] {
        def onCont(k: Input[E] => F[Step[F, E, A]]): F[Step[F, E2, A]] =
          F.pure(Step.cont((in: Input[E2]) => F.flatMap(k(in.map(f)))(loop)))
        def onDone(value: A): F[Step[F, E2, A]] = F.pure(Step.done(value))
        override def onEarly(value: A, remainder: Input[E]): F[Step[F, E2, A]] =
          F.pure(if (remainder.isEnd) Step.early(value, Input.end) else Step.done(value))
      }
    )

    Iteratee.iteratee(F.flatMap(state)(loop))
  }

  /**
   * Create a new [[Iteratee]] that first processes values with the given
   * [[Enumeratee]].
   */
  final def through[O](enumeratee: Enumeratee[F, O, E])(implicit F: Monad[F]): Iteratee[F, O, A] =
    Iteratee.joinI(Iteratee.iteratee(F.flatMap(state)(enumeratee(_))))

  /**
   * Transform the context of this [[Iteratee]].
   */
  final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit
    F: Functor[F]
  ): Iteratee[G, E, A] = {
    def transform: Step[F, E, A] => Step[G, E, A] = _.foldWith(
      new Step.Folder[F, E, A, Step[G, E, A]] {
        def onCont(k: Input[E] => F[Step[F, E, A]]): Step[G, E, A] =
          Step.cont(in => loop(k(in)))
        def onDone(value: A): Step[G, E, A] = Step.done(value)
        override def onEarly(value: A, remainder: Input[E]): Step[G, E, A] = Step.early(value, remainder)
      }
    )

    def loop: F[Step[F, E, A]] => G[Step[G, E, A]] = i => f(F.map(i)(transform))

    Iteratee.iteratee(loop(state))
  }

  /**
   * Lift this [[Iteratee]] into a different context.
   */
  final def up[G[_]](implicit G: Applicative[G], F: Comonad[F]): Iteratee[G, E, A] = mapI(
    new NaturalTransformation[F, G] {
      final def apply[A](a: F[A]): G[A] = G.pure(F.extract(a))
    }
  )

  /**
   * Create an [[Enumeratee]] that repeatedly applies this [[Iteratee]] to a
   * stream.
   *
   * The resulting enumeratee feeds input elements to this iteratee until it's
   * done and then feeds the produced value to the inner iteratee. The iteratee
   * will start over and loop until the inner iteratee is done.
   */
  final def sequenceI(implicit m: Monad[F]): Enumeratee[F, E, A] = Enumeratee.sequenceI(this)

  /**
   * Zip this [[Iteratee]] with another to create an iteratee that returns a
   * pair of their results.
   */
  final def zip[B](other: Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, (A, B)] =
    Iteratee.iteratee(
      F.flatMap(F.product(self.state, other.state)) {
        case (sA, sB) => Step.zip(sA, sB)
      }
    )

  /**
   * If this [[Iteratee]] has failed, use the provided function to recover.
   */
  final def handleErrorWith[T](f: T => Iteratee[F, E, A])(implicit F: MonadError[F, T]): Iteratee[F, E, A] =
    Iteratee.iteratee(F.handleErrorWith(state)(e => f(e).state))
}

/**
 * @groupname Constructors Constructors
 * @groupprio Constructors 0
 *
 * @groupname Utilities Miscellaneous utilities
 * @groupprio Utilities 1
 *
 * @groupname Collection Collection operation iteratees
 * @groupprio Collection 2
 */
final object Iteratee extends IterateeInstances {
  /**
   * Create an incomplete [[Iteratee]] that will use the given function to
   * process the next input.
   *
   * @group Constructors
   */
  final def cont[F[_]: Applicative, E, A](k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
    fromStep(Step.cont(in => k(in).state))

  /**
   * Create a new completed [[Iteratee]] with the given result and leftover
   * input.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](d: A): Iteratee[F, E, A] = fromStep(Step.done(d))

  /**
   * Create a new completed [[Iteratee]] with the given result and leftover
   * input.
   *
   * @group Constructors
   */
  final def early[F[_]: Applicative, E, A](d: A, r: Input[E]): Iteratee[F, E, A] = fromStep(Step.early(d, r))

  /**
   * Create an [[Iteratee]] from a [[io.iteratee.internal.Step]] in a context.
   *
   * @group Utilities
   */
  final def iteratee[F[_], E, A](s: F[Step[F, E, A]]): Iteratee[F, E, A] = new Iteratee[F, E, A](s)

  /**
   * Create an [[Iteratee]] from a [[io.iteratee.internal.Step]].
   *
   * @group Utilities
   */
  final def fromStep[F[_], E, A](s: Step[F, E, A])(implicit F: Applicative[F]): Iteratee[F, E, A] =
    iteratee(F.pure(s))

  /**
   * Lift an effectful value into an iteratee.
   *
   * @group Utilities
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    iteratee(Step.liftM(fa))

  /**
   * Create a failed iteratee with the given error.
   *
   * @group Utilities
   */
  final def fail[F[_], T, E, A](e: T)(implicit F: MonadError[F, T]): Iteratee[F, E, A] = liftM(F.raiseError[A](e))

  /**
   * An iteratee that reads nothing from a stream.
   *
   * @group Utilities
   */
  final def identity[F[_]: Applicative, E]: Iteratee[F, E, Unit] = done[F, E, Unit](())

  /**
   * Collapse an [[Iteratee]] returning a [[io.iteratee.internal.Step]] into one
   * layer.
   *
   * @group Utilities
   */
  final def joinI[F[_], E, I, B](it: Iteratee[F, E, Step[F, I, B]])(implicit F: Monad[F]): Iteratee[F, E, B] =
    iteratee(F.flatMap(it.state)(Step.joinI(_)))

  /**
   * An [[Iteratee]] that folds a stream using an initial value and an
   * accumulation function.
   *
   * @group Collection
   */
  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Iteratee[F, E, A] =
    Iteratee.fromStep(Step.fold[F, E, A](init)(f))

  /**
   * An [[Iteratee]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.fromStep(Step.foldM[F, E, A](init)(f))

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a vector.
   *
   * @group Collection
   */
  final def drain[F[_], A](implicit F: Applicative[F]): Iteratee[F, A, Vector[A]] =
    Iteratee.fromStep(Step.drain[F, A])

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a given
   * collection type.
   *
   * @group Collection
   */
  final def drainTo[F[_]: Monad, A, C[_]: Applicative: MonoidK]: Iteratee[F, A, C[A]] =
    Iteratee.fromStep(Step.drainTo[F, A, C])

  /**
   * An [[Iteratee]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] =
    Iteratee.fromStep(Step.head[F, E])

  /**
   * An [[Iteratee]] that returns the first value in a stream without consuming
   * it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] =
    Iteratee.fromStep(Step.peek[F, E])

  /**
   * An [[Iteratee]] that returns a given number of the first values in a
   * stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, A](n: Int): Iteratee[F, A, Vector[A]] =
    Iteratee.fromStep(Step.take[F, A](n))

  /**
   * An [[Iteratee]] that returns values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Iteratee[F, A, Vector[A]] =
    Iteratee.fromStep(Step.takeWhile[F, A](p))

  /**
   * An [[Iteratee]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Iteratee[F, E, Unit] =
    Iteratee.fromStep(Step.drop[F, E](n))

  /**
   * An [[Iteratee]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Iteratee[F, E, Unit] =
    Iteratee.fromStep(Step.dropWhile[F, E](p))

  /**
   * An [[Iteratee]] that collects all inputs in reverse order.
   *
   * @group Collection
   */
  final def reversed[F[_]: Applicative, A]: Iteratee[F, A, List[A]] =
    Iteratee.fold[F, A, List[A]](Nil)((acc, e) => e :: acc)

  /**
   * An [[Iteratee]] that combines values using an [[algebra.Monoid]] instance.
   *
   * @group Collection
   */
  final def length[F[_]: Applicative, E]: Iteratee[F, E, Int] = fold(0)((a, _) => a + 1)

  /**
   * An [[Iteratee]] that combines values using an [[algebra.Monoid]] instance.
   *
   * @group Collection
   */
  final def sum[F[_], E](implicit F: Monad[F], E: Monoid[E]): Iteratee[F, E, E] =
    fold(E.empty)((a, e) => E.combine(a, e))

  /**
   * An [[Iteratee]] that combines values using a function to a type with an
   * [[algebra.Monoid]] instance.
   *
   * @group Collection
   */
  final def foldMap[F[_], E, A](f: E => A)(implicit F: Monad[F], A: Monoid[A]): Iteratee[F, E, A] =
    fold(A.empty)((a, e) => A.combine(a, f(e)))

  /**
   * An [[Iteratee]] that checks if the stream is at its end.
   *
   * @group Collection
   */
  final def isEnd[F[_]: Applicative, E]: Iteratee[F, E, Boolean] =
    Iteratee.cont(in => Iteratee.early(in.isEnd, in))
}
