package io.iteratee
package internal

import cats.{ Applicative, Eval, Monad, Monoid, MonoidK, Semigroup }
import io.iteratee.NonEmptyVector
import cats.arrow.FunctionK

/**
 * Represents the current state of an [[io.iteratee.Iteratee]].
 *
 * @tparam F The effect type constructor
 * @tparam E The type of the input data
 * @tparam A The type of the result calculated by the [[io.iteratee.Iteratee]]
 */
sealed abstract class Step[F[_], E, A] extends Serializable {
  /**
   * Reduce this [[Step]] to a value using the given functions.
   */
  def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z

  def isDone: Boolean

  /**
   * Run this [[Step]] so that it produces a value in an effectful context.
   */
  def run: F[A]

  /**
   * Feed a single element to this [[Step]].
   */
  def feedEl(e: E): F[Step[F, E, A]]

  /**
   * Feed a multi-element input to this [[Step]].
   */
  def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]]

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B): Step[F, E, B]

  /**
   * Map a function over the inputs of this [[Step]].
   */
  def contramap[E2](f: E2 => E): Step[F, E2, A]

  /**
   * Transform the context of this [[Step]].
   */
  def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]]

  /**
   * Zip this [[Step]] with another.
   */
  def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)]
}

/**
 * @groupname Constructors Constructors
 * @groupprio Constructors 0
 *
 * @groupname Utilities Miscellaneous utilities
 * @groupprio Utilities 1
 *
 * @groupname Collection Collection operation steps
 * @groupprio Collection 2
 */
final object Step { self =>
  case class Done[F[_], E, A](
    value: A,
    remaining: Vector[E]
  )(implicit F: Applicative[F]) extends Step[F, E, A] {
    final def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z =
      ifDone(value, remaining)

    final def isDone: Boolean = true
    final def run: F[A] = F.pure(value)
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
    final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] = F.pure(this)

    final def map[B](f: A => B): Step[F, E, B] = Done(f(value), remaining)
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = Done(value, Vector.empty)
    final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = Done(value, remaining)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      M.flatMap(f(value)) {
        case Done(otherValue, otherRemaining) => F.pure(Done(otherValue, otherRemaining ++ remaining))
        case step => remaining match {
          case xs if xs.isEmpty => f(value)
          case xs if xs.size == 1 => step.feedEl(xs.head)
          case h +: t => step.feedChunk(h, NonEmptyVector.fromVectorUnsafe(t))
        }
      }

    final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = other match {
      case Done(otherValue, otherRemaining) => Done(
        (value, otherValue),
        if (remaining.size <= otherRemaining.size) remaining else otherRemaining
      )
      case step => step.map((value, _))
    }
  }

  private[internal] abstract class BaseCont[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] { self =>
    final def fold[Z](ifCont: (NonEmptyVector[E] => F[Step[F, E, A]]) => Z, ifDone: (A, Vector[E]) => Z): Z =
      ifCont { nev =>
        if (nev.length == 1) feedEl(nev.head) else feedChunk(nev.head, NonEmptyVector.fromVectorUnsafe(nev.tail))
      }

    final def isDone: Boolean = false

    final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = new Cont[G, E, A] {
      final def feedEl(e: E): G[Step[G, E, A]] = f(F.map(self.feedEl(e))(_.mapI(f)))
      final def feedChunk(h: E, t: NonEmptyVector[E]): G[Step[G, E, A]] = f(F.map(self.feedChunk(h, t))(_.mapI(f)))
      final def run: G[A] = f(self.run)
    }

    final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = other match {
      case Done(otherValue, _) => map((_, otherValue))
      case step => new Cont[F, E, (A, B)] {
        final def feedEl(e: E): F[Step[F, E, (A, B)]] = F.map2(self.feedEl(e), step.feedEl(e))(_.zip(_))
        final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, (A, B)]] =
          F.map2(self.feedChunk(h, t), step.feedChunk(h, t))(_.zip(_))
        final def run: F[(A, B)] = F.product(self.run, step.run)
      }
    }
  }

  abstract class Cont[F[_], E, A](implicit F: Applicative[F]) extends BaseCont[F, E, A] { self =>
    final def map[B](f: A => B): Step[F, E, B] = new Cont[F, E, B] {
      final def feedEl(e: E): F[Step[F, E, B]] = F.map(self.feedEl(e))(_.map(f))
      final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, B]] = F.map(self.feedChunk(h, t))(_.map(f))
      final def run: F[B] = F.map(self.run)(f)
    }

    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new Cont[F, E2, A] {
      final def feedEl(e: E2): F[Step[F, E2, A]] = F.map(self.feedEl(f(e)))(_.contramap(f))
      final def feedChunk(h: E2, t: NonEmptyVector[E2]): F[Step[F, E2, A]] =
        F.map(self.feedChunk(f(h), t.map(f)))(_.contramap(f))
      final def run: F[A] = self.run
    }

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Cont[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = M.flatMap(self.feedEl(e))(_.bind(f))
        final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, B]] =
          M.flatMap(self.feedChunk(h, t))(_.bind(f))
        final def run: F[B] = M.flatMap(self.run)(a => M.flatMap(f(a))(_.run))
      }
    )
  }

  final object Cont {
    abstract class WithValue[F[_], E, A](value: A)(implicit F: Applicative[F]) extends Cont[F, E, A] {
      final def run: F[A] = F.pure(value)
    }
  }

  private[this] abstract class PureCont[F[_], E, A](implicit F: Applicative[F]) extends BaseCont[F, E, A] { self =>
    protected def feedElPure(e: E): Step[F, E, A]
    protected def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, A]
    protected def runPure: A

    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(feedElPure(e))
    final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] = F.pure(feedChunkPure(h, t))
    final def run: F[A] = F.pure(runPure)

    final def map[B](f: A => B): Step[F, E, B] = new PureCont[F, E, B] {
      final def feedElPure(e: E): Step[F, E, B] = self.feedElPure(e).map(f)
      final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, B] = self.feedChunkPure(h, t).map(f)
      final def runPure: B = f(self.runPure)
    }

    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new PureCont[F, E2, A] {
      final def feedElPure(e: E2): Step[F, E2, A] = self.feedElPure(f(e)).contramap(f)
      final def feedChunkPure(h: E2, t: NonEmptyVector[E2]): Step[F, E2, A] =
        self.feedChunkPure(f(h), t.map(f)).contramap(f)
      final def runPure: A = self.runPure
    }

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Cont[F, E, B] {
        final def feedEl(e: E): F[Step[F, E, B]] = self.feedElPure(e).bind(f)
        final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, B]] = self.feedChunkPure(h, t).bind(f)
        final def run: F[B] = M.flatMap(f(self.runPure))(_.run)
      }
    )
  }

  private[this] object PureCont {
    abstract class WithValue[F[_], E, A](val runPure: A)(implicit F: Applicative[F]) extends PureCont[F, E, A]
  }

  /**
   * Create an incomplete [[Step]] that will use the given functions to process
   * the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](
    onInput: NonEmptyVector[E] => F[Step[F, E, A]],
    onEnd: F[A]
  )(implicit F: Applicative[F]): Step[F, E, A] = new Cont[F, E, A] {
    final def feedEl(e: E): F[Step[F, E, A]] = onInput(NonEmptyVector(e, Vector.empty))
    final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] = onInput(NonEmptyVector(h, t.toVector))
    final def run: F[A] = onEnd
  }

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A): Step[F, E, A] = Done(value, Vector.empty)

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def doneWithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Vector[E]): Step[F, E, A] =
    Done(value, remaining)

  /**
   * Lift an effectful value into a [[Step]].
   *
   * @group Utilities
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): F[Step[F, E, A]] = F.map(fa)(done(_))

  /**
   * Lift an effectful value in a [[cats.Eval]] into a [[Step]].
   *
   * @group Utilities
   */
  final def liftMEval[F[_], E, A](fa: Eval[F[A]])(implicit F: Monad[F]): F[Step[F, E, A]] = F.pure(
    new Cont[F, E, A] {
      final def run: F[A] = fa.value
      final def feedEl(e: E): F[Step[F, E, A]] = F.map(fa.value)(a => Done(a, Vector(e)))
      final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] =
        F.map(fa.value)(a => Done(a, h +: t.toVector))
    }
  )

  /**
   * Collapse a nested [[Step]] into one layer.
   *
   * @group Utilities
   */
  final def joinI[F[_], A, B, C](step: Step[F, A, Step[F, B, C]])(implicit F: Monad[F]): F[Step[F, A, C]] =
    step.bind {
      case Done(value, _) => F.pure(done(value))
      case next => F.map(next.run)(done(_))
    }

  /**
   * A [[Step]] that folds a stream using an initial value and an accumulation
   * function.
   *
   * @group Collection
   */
  final def fold[F[_], E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Step[F, E, A] =
    new PureCont.WithValue[F, E, A](init) {
      final def feedElPure(e: E): Step[F, E, A] = self.fold(f(init, e))(f)
      final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, A] = self.fold(t.foldLeft(f(init, h))(f))(f)
    }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] =
    new Cont.WithValue[F, E, A](init) {
      final def feedEl(e: E): F[Step[F, E, A]] = F.map(f(init, e))(a => foldM(a)(f))
      final def feedChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] =
        F.map(
          t.foldLeft(f(init, h))((fa, e) => F.flatMap(fa)(a => f(a, e)))
        )(a => foldM(a)(f))
    }

  /**
   * A [[Step]] that collects all the elements in a stream in a vector.
   *
   * @group Collection
   */
  final def consume[F[_]: Applicative, A]: Step[F, A, Vector[A]] = new ConsumeCont(Vector.empty)

  private[this] final class ConsumeCont[F[_], E](acc: Vector[E])(implicit F: Applicative[F])
      extends PureCont.WithValue[F, E, Vector[E]](acc) {
    final def feedElPure(e: E): Step[F, E, Vector[E]] = new ConsumeCont(acc :+ e)
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Vector[E]] =
      new ConsumeCont(acc ++ (h +: t.toVector))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def consumeIn[F[_], E, C[_]](implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ): Step[F, E, C[E]] = new ConsumeInCont(M.empty)

  private[this] final class ConsumeInCont[F[_], E, C[_]](acc: C[E])(implicit
    F: Applicative[F],
    M: MonoidK[C],
    C: Applicative[C]
  ) extends PureCont.WithValue[F, E, C[E]](acc) {
    final def feedElPure(e: E): Step[F, E, C[E]] = new ConsumeInCont(M.combineK(acc, C.pure(e)))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, C[E]] = new ConsumeInCont(
      t.foldLeft(M.combineK(acc, C.pure(h)))((a, e) => M.combineK(a, C.pure(e)))
    )
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def feedElPure(e: E): Step[F, E, Option[E]] = done(Some(e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Option[E]] = Done(Some(h), t.toVector)
    final def runPure: Option[E] = None
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def feedElPure(e: E): Step[F, E, Option[E]] = Done(Some(e), Vector(e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Option[E]] = Done(Some(h), h +: t.toVector)
    final def runPure: Option[E] = None
  }

  /**
   * A [[Step]] that counts the number of values in a stream.
   *
   * @group Collection
   */
  final def length[F[_]: Applicative, A]: Step[F, A, Long] = new LengthCont(0L)

  private[this] final class LengthCont[F[_], E](acc: Long)(implicit F: Applicative[F])
      extends PureCont.WithValue[F, E, Long](acc) {
    final def feedElPure(e: E): Step[F, E, Long] = new LengthCont(acc + 1L)
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Long] =
      new LengthCont(acc + 1L + t.toVector.size.toLong)
  }

  /**
   * A [[Step]] that sums of values in a stream.
   *
   * @group Collection
   */
  final def sum[F[_], E](implicit F: Applicative[F], M: Monoid[E]): Step[F, E, E] = new SumCont(M.empty)

  private[this] final class SumCont[F[_], E](acc: E)(implicit F: Applicative[F], M: Semigroup[E])
      extends PureCont.WithValue[F, E, E](acc) {
    final def feedElPure(e: E): Step[F, E, E] = new SumCont(M.combine(acc, e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, E] =
      new SumCont(M.combine(acc, M.combine(h, t.reduce)))
  }

  /**
   * A [[Step]] that transforms and sums values in a stream.
   *
   * @group Collection
   */
  final def foldMap[F[_], E, A](f: E => A)(implicit F: Applicative[F], M: Monoid[A]): Step[F, E, A] =
    new FoldMapCont(M.empty)(f)

  private[this] final class FoldMapCont[F[_], E, A](acc: A)(f: E => A)(implicit F: Applicative[F], M: Semigroup[A])
      extends PureCont.WithValue[F, E, A](acc) {
    final def feedElPure(e: E): Step[F, E, A] = new FoldMapCont(M.combine(acc, f(e)))(f)
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, A] =
      new FoldMapCont(M.combine(acc, M.combine(f(h), t.map(f).reduce)))(f)
  }

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, E](n: Int): Step[F, E, Vector[E]] =
    if (n <= 0) done[F, E, Vector[E]](Vector.empty) else new TakeCont(Vector.empty, n)

  private[this] final class TakeCont[F[_], E](acc: Vector[E], n: Int)(implicit F: Applicative[F])
      extends PureCont.WithValue[F, E, Vector[E]](acc) {
    final def feedElPure(e: E): Step[F, E, Vector[E]] = if (n == 1) done(acc :+ e) else new TakeCont(acc :+ e, n - 1)
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Vector[E]] = {
      val v = h +: t.toVector
      val diff = n - v.size

      if (diff > 0) new TakeCont(acc ++ v, diff) else if (diff == 0) done(acc ++ v) else {
        val (taken, left) = v.splitAt(n)

        Done(acc ++ taken, left)
      }
    }
  }

  /**
   * A [[Step]] that returns values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Vector[E]] =
    new TakeWhileCont(Vector.empty, p)

  private[this] final class TakeWhileCont[F[_], E](acc: Vector[E], p: E => Boolean)(implicit F: Applicative[F])
      extends PureCont.WithValue[F, E, Vector[E]](acc) {
    final def feedElPure(e: E): Step[F, E, Vector[E]] =
      if (p(e)) new TakeWhileCont(acc :+ e, p) else Done(acc, Vector(e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Vector[E]] = {
      val (before, after) = (h +: t.toVector).span(p)

      if (after.isEmpty) new TakeWhileCont(acc ++ before, p) else
        Done(acc ++ before, after)
    }
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_], E](n: Int)(implicit F: Applicative[F]): Step[F, E, Unit] =
    if (n <= 0) done(()) else new PureCont[F, E, Unit] {
      final def feedElPure(e: E): Step[F, E, Unit] = drop(n - 1)
      final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Unit] = {
        val len = t.toVector.size + 1

        if (len <= n) drop(n - len) else {
          Done((), (h +: t.toVector).drop(n))
        }
      }
      final def runPure: Unit = ()
    }

  /**
   * A [[Step]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Step[F, E, Unit] =
    new DropWhileCont(p)

  private[this] final class DropWhileCont[F[_], E](p: E => Boolean)(implicit F: Applicative[F])
      extends PureCont[F, E, Unit] {
    final def feedElPure(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else Done((), Vector(e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Unit] = {
      val after = (h +: t.toVector).dropWhile(p)

      if (after.isEmpty) dropWhile(p) else Done((), after)
    }
    final def runPure: Unit = ()
  }

  final def isEnd[F[_], E](implicit F: Applicative[F]): Step[F, E, Boolean] = new PureCont[F, E, Boolean] {
    final def feedElPure(e: E): Step[F, E, Boolean] = Done(false, Vector(e))
    final def feedChunkPure(h: E, t: NonEmptyVector[E]): Step[F, E, Boolean] = Done(false, h +: t.toVector)
    final def runPure: Boolean = true
  }
}
