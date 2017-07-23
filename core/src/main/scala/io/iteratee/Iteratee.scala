package io.iteratee

import cats.{ Applicative, Comonad, Eval, Functor, Monad, MonadError, Monoid, MonoidK, Semigroup }
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import io.iteratee.internal.Step

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
   * Advance this [[Iteratee]] with the given [[Enumerator]].
   */
  final def apply(enumerator: Enumerator[F, E])(implicit F: Monad[F]): Iteratee[F, E, A] =
    Iteratee.iteratee(F.flatMap(state)(enumerator(_)))

  /**
   * Reduce this [[Iteratee]] to an effectful value using the given functions.
   */
  final def fold[Z](ifCont: (NonEmptyList[E] => Iteratee[F, E, A]) => Z, ifDone: (A, List[E]) => Z)
    (implicit F: Functor[F]): F[Z] = F.map(state)(_.fold(f => ifCont(in => Iteratee.iteratee(f(in))), ifDone))

  /**
   * Run this iteratee and close the stream so that it must produce an effectful
   * value.
   */
  final def run(implicit F: Monad[F]): F[A] = F.flatMap(state)(_.run)

  /**
   * Map a function over the result of this [[Iteratee]].
   */
  final def map[B](f: A => B)(implicit F: Functor[F]): Iteratee[F, E, B] = Iteratee.iteratee(F.map(state)(_.map(f)))

  /**
   * Replace the result of this [[Iteratee]].
   */
  final def as[B](b: B)(implicit F: Functor[F]): Iteratee[F, E, B] = Iteratee.iteratee(F.map(state)(_.as(b)))

  /**
   * Map a monadic function over the result of this [[Iteratee]].
   */
  final def flatMapM[B](f: A => F[B])(implicit F: Monad[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.flatMap(state)(_.bind(a => F.map(f(a))(Step.done[F, E, B](_)))))

  /**
   * Map a function returning an [[Iteratee]] over the result.
   */
  final def flatMap[B](f: A => Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, B] =
    Iteratee.iteratee(F.flatMap(state)(_.bind(a => f(a).state)))

  /**
   * Transform the inputs to this [[Iteratee]].
   */
  final def contramap[E2](f: E2 => E)(implicit F: Functor[F]): Iteratee[F, E2, A] =
    Iteratee.iteratee(F.map(state)(_.contramap(f)))

  /**
   * Create a new [[Iteratee]] that first processes values with the given
   * [[Enumeratee]].
   */
  final def through[O](enumeratee: Enumeratee[F, O, E])(implicit F: Monad[F]): Iteratee[F, O, A] =
    Iteratee.joinI(Iteratee.iteratee(F.flatMap(state)(enumeratee(_))))

  /**
   * Transform the context of this [[Iteratee]].
   */
  final def mapI[G[_]: Applicative](f: FunctionK[F, G])(implicit F: Applicative[F]): Iteratee[G, E, A] =
    Iteratee.iteratee(f(F.map(state)(_.mapI(f))))

  /**
   * Lift this [[Iteratee]] into a different context.
   */
  final def up[G[_]](implicit G: Applicative[G], F: Comonad[F], F0: Applicative[F]): Iteratee[G, E, A] = mapI(
    new FunctionK[F, G] {
      final def apply[A](a: F[A]): G[A] = G.pure(F.extract(a))
    }
  )

  /**
   * Zip this [[Iteratee]] with another to create an iteratee that returns a
   * pair of their results.
   */
  final def zip[B](other: Iteratee[F, E, B])(implicit F: Applicative[F]): Iteratee[F, E, (A, B)] =
    Iteratee.iteratee(F.map2(self.state, other.state)(_.zip(_)))

  /**
   * If this [[Iteratee]] has failed, use the provided function to recover.
   */
  final def handleErrorWith[T](f: T => Iteratee[F, E, A])(implicit F: MonadError[F, T]): Iteratee[F, E, A] =
    Iteratee.iteratee(F.handleErrorWith(state)(e => f(e).state))

  /**
   * Create a new [[Iteratee]] that throws away the value this one returns.
   */
  final def discard(implicit F: Functor[F]): Iteratee[F, E, Unit] = as(())

  /**
   * Ensure that an action will be performed when this iteratee is done, whether
   * or not it succeeds.
   */
  final def ensure[T](action: F[Unit])(implicit F: MonadError[F, T]): Iteratee[F, E, A] =
    ensureEval(Eval.now(action))

  /**
   * Ensure that an action will be performed when this iteratee is done, whether
   * or not it succeeds.
   */
  final def ensureEval[T](action: Eval[F[Unit]])(implicit F: MonadError[F, T]): Iteratee[F, E, A] =
    handleErrorWith[T](_ => Iteratee.iteratee(F.flatMap(action.value)(_ => state))).flatMapM(result =>
      F.map(action.value)(_ => result)
    )
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
  final def cont[F[_]: Applicative, E, A](
    ifInput: NonEmptyList[E] => Iteratee[F, E, A],
    ifEnd: F[A]
  ): Iteratee[F, E, A] = fromStep(Step.cont(es => ifInput(es).state, ifEnd))

  /**
   * Create a new completed [[Iteratee]] with the given result.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A): Iteratee[F, E, A] = fromStep(Step.done(value))

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
   * Lift an effectful value into an iteratee.
   *
   * @group Utilities
   */
  final def liftMEval[F[_], E, A](fa: Eval[F[A]])(implicit F: Monad[F]): Iteratee[F, E, A] =
    iteratee(Step.liftMEval(fa))

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
  final def consume[F[_]: Applicative, A]: Iteratee[F, A, Vector[A]] = fromStep(Step.consume[F, A])

  /**
   * An [[Iteratee]] that collects all the elements in a stream in a given
   * collection type.
   *
   * @group Collection
   */
  final def consumeIn[F[_]: Applicative, A, C[_]: Applicative: MonoidK]: Iteratee[F, A, C[A]] =
    fromStep(Step.consumeIn[F, A, C])

  /**
   * An [[Iteratee]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = fromStep(Step.head[F, E])

  /**
   * An [[Iteratee]] that returns the first value in a stream without consuming
   * it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = fromStep(Step.peek[F, E])

  /**
   * An [[Iteratee]] that returns the last value in a stream.
   *
   * @group Collection
   */
  final def last[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = fromStep(Step.last[F, E])

  /**
   * An [[Iteratee]] that returns a given number of the first values in a
   * stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, A](n: Int): Iteratee[F, A, Vector[A]] = fromStep(Step.take[F, A](n))

  /**
   * An [[Iteratee]] that returns values from a stream as long as they satisfy
   * the given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Iteratee[F, A, Vector[A]] =
    fromStep(Step.takeWhile[F, A](p))

  /**
   * An [[Iteratee]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Iteratee[F, E, Unit] = fromStep(Step.drop[F, E](n))

  /**
   * An [[Iteratee]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Iteratee[F, E, Unit] =
    fromStep(Step.dropWhile[F, E](p))

  /**
   * An [[Iteratee]] that collects all inputs in reverse order.
   *
   * @group Collection
   */
  final def reversed[F[_]: Applicative, A]: Iteratee[F, A, List[A]] =
    fold[F, A, List[A]](Nil)((acc, e) => e :: acc)

  /**
   * An [[Iteratee]] that counts the number of values in a stream.
   *
   * @group Collection
   */
  final def length[F[_]: Applicative, E]: Iteratee[F, E, Long] = fromStep(Step.length[F, E])

  /**
   * An [[Iteratee]] that combines values using an [[cats.Monoid]] instance.
   *
   * @group Collection
   */
  final def sum[F[_]: Applicative, E: Monoid]: Iteratee[F, E, E] = fromStep(Step.sum[F, E])

  /**
   * An [[Iteratee]] that combines values using a function to a type with a
   * [[cats.Monoid]] instance.
   *
   * @group Collection
   */
  final def foldMap[F[_]: Applicative, E, A: Monoid](f: E => A): Iteratee[F, E, A] = Iteratee.fromStep(Step.foldMap(f))

  /**
   * An [[Iteratee]] that combines values using an effectful function to a type
   * with a [[cats.Monoid]] instance.
   *
   * @group Collection
   */
  final def foldMapM[F[_]: Applicative, E, A: Monoid](f: E => F[A]): Iteratee[F, E, A] =
    Iteratee.fromStep(Step.foldMapM(f))

  /**
   * An [[Iteratee]] that combines values using a function to a type with a
   * [[cats.Semigroup]] instance.
   *
   * @group Collection
   */
  final def foldMapOption[F[_], E, A](f: E => A)(implicit
    F: Applicative[F],
    A: Semigroup[A]
  ): Iteratee[F, E, Option[A]] =
    foldMap[F, E, Option[A]](e => Some(f(e)))(F, cats.kernel.instances.option.catsKernelStdMonoidForOption(A))

  /**
   * An [[Iteratee]] that combines values using an effectful function to a type
   * with a [[cats.Semigroup]] instance.
   *
   * @group Collection
   */
  final def foldMapMOption[F[_], E, A](f: E => F[A])(implicit
    F: Applicative[F],
    A: Semigroup[A]
  ): Iteratee[F, E, Option[A]] =
    foldMapM[F, E, Option[A]](e =>
      F.map(f(e))(Some(_)))(F, cats.kernel.instances.option.catsKernelStdMonoidForOption(A)
    )

  /**
   * An [[Iteratee]] that checks if the stream is at its end.
   *
   * @group Collection
   */
  final def isEnd[F[_]: Applicative, E]: Iteratee[F, E, Boolean] = Iteratee.fromStep(Step.isEnd)

  /**
   * An [[Iteratee]] that runs a function for its side effects.
   */
  def foreach[F[_]: Applicative, A](f: A => Unit): Iteratee[F, A, Unit] = fold(())((_, a) => f(a))

  /**
   * An [[Iteratee]] that runs an effectful function for its side effects.
   */
  def foreachM[F[_]: Monad, A](f: A => F[Unit]): Iteratee[F, A, Unit] = foldM(())((_, a) => f(a))
}
