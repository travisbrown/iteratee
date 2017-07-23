package io.iteratee
package internal

import cats.{ Applicative, Eval, Monad, Monoid, MonoidK, Semigroup }
import cats.arrow.FunctionK
import cats.data.NonEmptyList

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
  def fold[Z](ifCont: (NonEmptyList[E] => F[Step[F, E, A]]) => Z, ifDone: (A, List[E]) => Z): Z

  def isDone: Boolean

  /**
   * Run this [[Step]] so that it produces a value in an effectful context.
   */
  def run: F[A]

  /**
   * Feed a chunk (possibly empty) to this [[Step]].
   *
   * If the chunk is empty, this method will return the [[Step]] itself in the
   * context `F`.
   */
  def feed(chunk: Seq[E]): F[Step[F, E, A]]

  /**
   * Feed a single element to this [[Step]].
   *
   * Must be consistent with `feed` and `feedNonEmpty`.
   */
  def feedEl(e: E): F[Step[F, E, A]]

  /**
   * Feed a chunk that is known to be non-empty to this [[Step]].
   *
   * Note that this method is unsafe! If you do not know that the chunk contains
   * at least one element, you must call `feed` instead.
   */
  protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]]

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B): Step[F, E, B]

  /**
   * Replace the value of this [[Step]].
   */
  def as[B](b: B): Step[F, E, B]

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
  private[this] final class Done[F[_], E, A](val value: A)(private[internal] val remaining: Seq[E])(implicit
    F: Applicative[F]
  ) extends Step[F, E, A] {
    final def fold[Z](ifCont: (NonEmptyList[E] => F[Step[F, E, A]]) => Z, ifDone: (A, List[E]) => Z): Z =
      ifDone(value, remaining.toList)

    final def isDone: Boolean = true
    final def run: F[A] = F.pure(value)
    final def feed(chunk: Seq[E]): F[Step[F, E, A]] = F.pure(this)
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(this)
    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] = F.pure(this)

    final def map[B](f: A => B): Step[F, E, B] = doneWithLeftovers(f(value), remaining)
    final def as[B](b: B): Step[F, E, B] = doneWithLeftovers(b, remaining)
    final def contramap[E2](f: E2 => E): Step[F, E2, A] = done(value)
    final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = doneWithLeftovers(value, remaining)

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] =
      M.flatMap(f(value)) { next =>
        if (next.isDone) {
          F.pure(doneWithLeftovers(next.asInstanceOf[Done[F, E, B]].value, remaining))
        } else {
          val c = remaining.lengthCompare(1)

          if (c < 0) F.pure(next) else if (c == 0) next.feedEl(remaining(0)) else next.feedNonEmpty(remaining)
        }
      }

    final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = if (other.isDone) {
      val asDone = other.asInstanceOf[Done[F, E, B]]
      val newRemaining = if (remaining.size <= asDone.remaining.size) remaining else asDone.remaining

      doneWithLeftovers((value, asDone.value), newRemaining)
    } else {
      other.map((value, _))
    }
  }

  private[internal] abstract class BaseCont[F[_], E, A](implicit F: Applicative[F]) extends Step[F, E, A] { self =>
    final def fold[Z](ifCont: (NonEmptyList[E] => F[Step[F, E, A]]) => Z, ifDone: (A, List[E]) => Z): Z =
      ifCont { nev =>
        if (nev.tail.isEmpty) feedEl(nev.head) else feedNonEmpty(nev.toList)
      }

    final def isDone: Boolean = false

    final def feed(chunk: Seq[E]): F[Step[F, E, A]] = {
      val c = chunk.lengthCompare(1)

      if (c < 0) F.pure(this) else if (c == 0) feedEl(chunk(0)) else feedNonEmpty(chunk)
    }

    final def as[B](b: B): Step[F, E, B] = map(_ => b)

    final def mapI[G[_]: Applicative](f: FunctionK[F, G]): Step[G, E, A] = new Cont[G, E, A] {
      final def run: G[A] = f(self.run)
      final def feedEl(e: E): G[Step[G, E, A]] = f(F.map(self.feedEl(e))(_.mapI(f)))
      final protected def feedNonEmpty(chunk: Seq[E]): G[Step[G, E, A]] =
        f(F.map(self.feedNonEmpty(chunk))(_.mapI(f)))
    }

    final def zip[B](other: Step[F, E, B]): Step[F, E, (A, B)] = if (other.isDone) {
      map((_, other.asInstanceOf[Done[F, E, B]].value))
    } else {
      new Cont[F, E, (A, B)] {
        final def run: F[(A, B)] = F.product(self.run, other.run)
        final def feedEl(e: E): F[Step[F, E, (A, B)]] = F.map2(self.feedEl(e), other.feedEl(e))(_.zip(_))
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, (A, B)]] =
          F.map2(self.feedNonEmpty(chunk), other.feedNonEmpty(chunk))(_.zip(_))
      }
    }
  }

  abstract class Cont[F[_], E, A](implicit F: Applicative[F]) extends BaseCont[F, E, A] { self =>
    final def map[B](f: A => B): Step[F, E, B] = new Cont[F, E, B] {
      final def run: F[B] = F.map(self.run)(f)
      final def feedEl(e: E): F[Step[F, E, B]] = F.map(self.feedEl(e))(_.map(f))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, B]] =
        F.map(self.feedNonEmpty(chunk))(_.map(f))
    }

    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new Cont[F, E2, A] {
      final def run: F[A] = self.run
      final def feedEl(e: E2): F[Step[F, E2, A]] = F.map(self.feedEl(f(e)))(_.contramap(f))
      final protected def feedNonEmpty(chunk: Seq[E2]): F[Step[F, E2, A]] =
       F.map(self.feedNonEmpty(chunk.map(f)))(_.contramap(f))
    }

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Cont[F, E, B] {
        final def run: F[B] = M.flatMap(self.run)(a => M.flatMap(f(a))(_.run))
        final def feedEl(e: E): F[Step[F, E, B]] = M.flatMap(self.feedEl(e))(_.bind(f))
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, B]] =
          M.flatMap(self.feedNonEmpty(chunk))(_.bind(f))
      }
    )
  }

  final object Cont {
    abstract class WithValue[F[_], E, A](value: A)(implicit F: Applicative[F]) extends Cont[F, E, A] {
      final def run: F[A] = F.pure(value)
    }
  }

  private[this] abstract class PureCont[F[_], E, A](implicit F: Applicative[F]) extends BaseCont[F, E, A] { self =>
    protected def runPure: A
    protected def feedElPure(e: E): Step[F, E, A]
    protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, A]

    final def run: F[A] = F.pure(runPure)
    final def feedEl(e: E): F[Step[F, E, A]] = F.pure(feedElPure(e))
    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] = F.pure(feedNonEmptyPure(chunk))

    final def map[B](f: A => B): Step[F, E, B] = new PureCont[F, E, B] {
      final def runPure: B = f(self.runPure)
      final def feedElPure(e: E): Step[F, E, B] = self.feedElPure(e).map(f)
      final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, B] = self.feedNonEmptyPure(chunk).map(f)
    }

    final def contramap[E2](f: E2 => E): Step[F, E2, A] = new PureCont[F, E2, A] {
      final def runPure: A = self.runPure
      final def feedElPure(e: E2): Step[F, E2, A] = self.feedElPure(f(e)).contramap(f)
      final protected def feedNonEmptyPure(chunk: Seq[E2]): Step[F, E2, A] =
        self.feedNonEmptyPure(chunk.map(f)).contramap(f)
    }

    final def bind[B](f: A => F[Step[F, E, B]])(implicit M: Monad[F]): F[Step[F, E, B]] = F.pure(
      new Cont[F, E, B] {
        final def run: F[B] = M.flatMap(f(self.runPure))(_.run)
        final def feedEl(e: E): F[Step[F, E, B]] = self.feedElPure(e).bind(f)
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, B]] = self.feedNonEmptyPure(chunk).bind(f)
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
    onInput: NonEmptyList[E] => F[Step[F, E, A]],
    onEnd: F[A]
  )(implicit F: Applicative[F]): Step[F, E, A] = new Cont[F, E, A] {
    final def run: F[A] = onEnd
    final def feedEl(e: E): F[Step[F, E, A]] = onInput(NonEmptyList(e, Nil))
    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] =
      onInput(NonEmptyList.fromListUnsafe(chunk.toList))
  }

  /**
   * Create a new completed [[Step]] with the given result.
   *
   * @group Constructors
   */
  final def done[F[_]: Applicative, E, A](value: A): Step[F, E, A] = new Done(value)(Nil)

  /**
   * Create a new completed [[Step]] with the given result and leftover input.
   *
   * @group Constructors
   */
  final def doneWithLeftovers[F[_]: Applicative, E, A](value: A, remaining: Seq[E]): Step[F, E, A] =
    new Done(value)(remaining)

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
      final def feedEl(e: E): F[Step[F, E, A]] = F.map(fa.value)(doneWithLeftovers(_, e :: Nil))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] = F.map(fa.value)(doneWithLeftovers(_, chunk))
    }
  )

  /**
   * Collapse a nested [[Step]] into one layer.
   *
   * @group Utilities
   */
  final def joinI[F[_], A, B, C](step: Step[F, A, Step[F, B, C]])(implicit F: Monad[F]): F[Step[F, A, C]] =
    step.bind { next =>
      if (next.isDone) F.pure(done(next.asInstanceOf[Done[F, B, C]].value)) else F.map(next.run)(done(_))
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
      final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, A] = self.fold(chunk.foldLeft(init)(f))(f)
    }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] =
    new Cont.WithValue[F, E, A](init) {
      final def feedEl(e: E): F[Step[F, E, A]] = F.map(f(init, e))(foldM(_)(f))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] =
        F.map(chunk.tail.foldLeft(f(init, chunk.head))((fa, e) => F.flatMap(fa)(a => f(a, e))))(foldM(_)(f))
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Vector[E]] = new ConsumeCont(acc ++ chunk)
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, C[E]] = new ConsumeInCont(
      chunk.foldLeft(acc)((a, e) => M.combineK(a, C.pure(e)))
    )
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def runPure: Option[E] = None
    final def feedElPure(e: E): Step[F, E, Option[E]] = done(Some(e))
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Option[E]] =
      doneWithLeftovers(Some(chunk.head), chunk.tail)
  }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new PureCont[F, E, Option[E]] {
    final def runPure: Option[E] = None
    final def feedElPure(e: E): Step[F, E, Option[E]] = doneWithLeftovers(Some(e), e :: Nil)
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Option[E]] =
      doneWithLeftovers(Some(chunk.head), chunk)
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def last[F[_], E](implicit F: Applicative[F]): Step[F, E, Option[E]] = new LastCont(None)

  private[this] final class LastCont[F[_], E](acc: Option[E])(implicit F: Applicative[F])
      extends PureCont.WithValue[F, E, Option[E]](acc) {
    final def feedElPure(e: E): Step[F, E, Option[E]] = new LastCont(Some(e))
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Option[E]] = new LastCont(Some(chunk.last))
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Long] = new LengthCont(acc + chunk.size.toLong)
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, E] =
      new SumCont(M.combine(acc, chunk.reduce(M.combine)))
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, A] =
      new FoldMapCont(M.combine(acc, chunk.map(f).reduce(M.combine)))(f)
  }

  /**
   * A [[Step]] that transforms and sums values in a stream.
   *
   * @group Collection
   */
  final def foldMapM[F[_], E, A](f: E => F[A])(implicit F: Applicative[F], M: Monoid[A]): Step[F, E, A] =
    new FoldMapMCont(M.empty)(f)

  private[this] final class FoldMapMCont[F[_], E, A](acc: A)(f: E => F[A])(implicit F: Applicative[F], M: Semigroup[A])
      extends Cont.WithValue[F, E, A](acc) {
    final def feedEl(e: E): F[Step[F, E, A]] = F.map(f(e))(a => new FoldMapMCont(M.combine(acc, a))(f))
    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] = F.map(
      chunk.foldLeft(F.pure(acc)) {
        case (acc, e) => F.map2(acc, f(e))(M.combine)
      }
    )(new FoldMapMCont(_)(f))
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
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Vector[E]] = {
      val diff = n - chunk.size

      if (diff > 0) new TakeCont(acc ++ chunk, diff) else if (diff == 0) done(acc ++ chunk) else {
        val (taken, left) = chunk.splitAt(n)

        doneWithLeftovers(acc ++ taken, left)
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
      if (p(e)) new TakeWhileCont(acc :+ e, p) else doneWithLeftovers(acc, e :: Nil)
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Vector[E]] = {
      val (before, after) = chunk.span(p)

      if (after.isEmpty) new TakeWhileCont(acc ++ before, p) else doneWithLeftovers(acc ++ before, after)
    }
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_], E](n: Int)(implicit F: Applicative[F]): Step[F, E, Unit] =
    if (n <= 0) done(()) else new PureCont[F, E, Unit] {
      final def runPure: Unit = ()
      final def feedElPure(e: E): Step[F, E, Unit] = drop(n - 1)
      final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Unit] = {
        val len = chunk.size

        if (len <= n) drop(n - len) else doneWithLeftovers((), chunk.drop(n))
      }
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
    final def runPure: Unit = ()
    final def feedElPure(e: E): Step[F, E, Unit] = if (p(e)) dropWhile(p) else doneWithLeftovers((), e :: Nil)
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Unit] = {
      val after = chunk.dropWhile(p)

      if (after.isEmpty) dropWhile(p) else doneWithLeftovers((), after)
    }
  }

  final def isEnd[F[_], E](implicit F: Applicative[F]): Step[F, E, Boolean] = new PureCont[F, E, Boolean] {
    final def runPure: Boolean = true
    final def feedElPure(e: E): Step[F, E, Boolean] = doneWithLeftovers(false, e :: Nil)
    final protected def feedNonEmptyPure(chunk: Seq[E]): Step[F, E, Boolean] = doneWithLeftovers(false, chunk)
  }

  private[this] class TailRecMCont[F[_], E, A, B](a: A)(f: A => F[Step[F, E, Either[A, B]]])(implicit
    F: Monad[F]
  ) extends Step.Cont[F, E, B] {
    final def run: F[B] = F.tailRecM[A, B](a)(a => F.flatMap(f(a))(_.run))

    private[this] def loop(s: Step[F, E, Either[A, B]]): F[Either[Step[F, E, Either[A, B]], Step[F, E, B]]] =
      if (s.isDone) {
        val asDone = s.asInstanceOf[Done[F, E, Either[A, B]]]

        asDone.value match {
          case Right(b) => F.pure(Right(s.as(b)))
          case Left(a) =>
            val c = asDone.remaining.lengthCompare(1)

            if (c < 0) {
              F.pure(Right(new TailRecMCont(a)(f)))
            } else if (c == 0) {
              F.flatMap(f(a))(s => F.map(s.feedEl(asDone.remaining.head))(Left(_)))
            } else {
              F.flatMap(f(a))(s => F.map(s.feedNonEmpty(asDone.remaining))(Left(_)))
            }
        }
      } else {
        F.map(
          s.bind[B] {
            case Right(b) => F.pure(done(b))
            case Left(a) => F.pure(new TailRecMCont(a)(f))
          }
        )(Right(_))
      }

    final def feedEl(e: E): F[Step[F, E, B]] =
      F.flatMap(f(a))(s => F.flatMap(s.feedEl(e))(F.tailRecM(_)(loop)))

    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, B]] =
      F.flatMap(f(a))(s => F.flatMap(s.feedNonEmpty(chunk))(F.tailRecM(_)(loop)))
  }

  final def tailRecM[F[_]: Monad, E, A, B](a: A)(f: A => F[Step[F, E, Either[A, B]]]): Step[F, E, B] =
    new TailRecMCont(a)(f)
}
