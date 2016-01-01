package io.iteratee.internal

import algebra.Monoid
import cats.{ Applicative, Functor, Monad, MonoidK }
import io.iteratee.Iteratee

/**
 * Represents the current state of an [[Iteratee]].
 *
 * An [[Iteratee]] has either already calculated a result ([[Step.done]]) or is
 * waiting for more data ([[Step.cont]]).
 *
 * @tparam F The effect type constructor
 * @tparam E The type of the input data
 * @tparam A The type of the result calculated by the [[Iteratee]]
 */
sealed abstract class Step[F[_], E, A] extends Serializable {
  /**
   * The [[Iteratee]]'s result.
   *
   * In some cases we know that an iteratee has been constructed in such a way
   * that it must be in a completed state, even though that's not tracked by the
   * type system. This method provides (unsafe) access to the result for use in
   * these situations.
   */
  private[iteratee] def unsafeValue: A

  /**
   * Reduce this [[Step]] to a value using the given functions.
   */
  final def fold[Z](
    cont: (List[E] => F[Step[F, E, A]]) => Z,
    done: A => Z,
    early: (A, List[E]) => Z
  ): Z = foldWith(
    new Step.Folder[F, E, A, Z] {
      final def onCont(k: List[E] => F[Step[F, E, A]]): Z = cont(k)
      final def onDone(value: A): Z = done(value)
      override final def onEarly(value: A, leftover: List[E]): Z = early(value, leftover)
    }
  )

  /**
   * Reduce this [[Step]] to a value using the given pair of functions.
   *
   * This method is provided primarily for internal use and for cases where the
   * expense of allocating multiple function objects and collection instances is
   * known to be too high. In most cases [[fold]] should be preferred.
   */
  def foldWith[Z](folder: Step.Folder[F, E, A, Z]): Z

  def isFinished: Boolean

  /**
   * Map a function over the value of this [[Step]].
   */
  def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B]

  /**
   * Map a function returning a [[Step]] in a monadic context over the value of
   * this [[Step]] and flatten the result.
   */
  def bindF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]]

  /**
   * Apply this [[Step]] to an input.
   */
  def feed(in: List[E])(implicit F: Applicative[F]): F[Step[F, E, A]]
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
final object Step {
  /**
   * Represents a pair of functions that can be used to reduce a [[Step]] to a
   * value.
   *
   * Combining two "functions" into a single class allows us to avoid
   * allocations.
   *
   * @group Utilities
   *
   * @tparam E The type of the input data
   * @tparam F The effect type constructor
   * @tparam A The type of the result calculated by the [[Iteratee]]
   * @tparam B The type of the result of the fold
   */
  abstract class Folder[F[_], E, A, Z] extends Serializable {
    def onCont(k: List[E] => F[Step[F, E, A]]): Z
    def onDone(value: A): Z
    def onEarly(value: A, leftover: List[E]): Z = onDone(value)
  }

  /**
   * Create an incomplete state that will use the given function to process the next input.
   *
   * @group Constructors
   */
  final def cont[F[_], E, A](k: List[E] => F[Step[F, E, A]]): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = diverge[A]
    final def isFinished: Boolean = false
    final def foldWith[B](folder: Folder[F, E, A, B]): B = folder.onCont(k)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = cont(in =>
      F.map(k(in))(_.map(f))
    )
    final def bindF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(u => F.flatMap(k(u))(_.bindF(f)))
    )
    final def feed(in: List[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = k(in)
  }

  /**
   * Create an incomplete state that will use the given pure function to process the next input.
   *
   * @group Constructors
   */
  final def pureCont[F[_], E, A](k: List[E] => Step[F, E, A])(implicit
    F0: Applicative[F]
  ): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = diverge[A]
    final def isFinished: Boolean = false
    final def foldWith[B](folder: Folder[F, E, A, B]): B = folder.onCont(in => F0.pure(k(in)))
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = pureCont(in =>
      k(in).map(f)
    )
    final def bindF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(in => k(in).bindF(f))
    )
    final def feed(in: List[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(k(in))
  }

  /**
   * Create a new completed state with the given result and leftover input.
   *
   * @group Constructors
   */
  final def done[F[_], E, A](value: A): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = value
    final def isFinished: Boolean = true
    final def foldWith[B](folder: Folder[F, E, A, B]): B = folder.onDone(value)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = done(f(value))
    final def bindF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = f(value)
    final def feed(in: List[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(this)
  }

  /**
   * Create a new completed state with the given result and leftover input.
   *
   * @group Constructors
   */
  final def early[F[_], E, A](value: A, leftover: List[E]): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = value
    final def isFinished: Boolean = true
    final def foldWith[B](folder: Folder[F, E, A, B]): B = folder.onEarly(value, leftover)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = early(f(value), leftover)

    final def bindF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] =
      F.flatMap(f(value))(
        _.foldWith(
          new Folder[F, E, B, F[Step[F, E, B]]] {
            final def onCont(k: List[E] => F[Step[F, E, B]]): F[Step[F, E, B]] = k(leftover)
            final def onDone(aa: B): F[Step[F, E, B]] = F.pure(early(aa, leftover))
            override final def onEarly(aa: B, r: List[E]): F[Step[F, E, B]] = F.pure(early(aa, leftover))
          }
        )
      )

    final def feed(in: List[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(this)
  }

  /**
   * Lift a monadic value into a [[Step]].
   *
   * @group Utilities
   */
  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): F[Step[F, E, A]] = F.map(fa)(done)

  /**
   * Collapse a nested [[Step]] into one layer.
   *
   * @group Utilities
   */
  final def joinI[F[_], A, B, C](step: Step[F, A, Step[F, B, C]])(implicit F: Monad[F]): F[Step[F, A, C]] = {
    def check: Step[F, B, C] => F[Step[F, A, C]] = _.foldWith(
      new Folder[F, B, C, F[Step[F, A, C]]] {
        final def onCont(k: List[B] => F[Step[F, B, C]]): F[Step[F, A, C]] = F.flatMap(k(Nil))(
          s => if (s.isFinished) check(s) else diverge
        )
        final def onDone(value: C): F[Step[F, A, C]] = F.pure(done(value))
      }
    )

    step.bindF(check)
  }

  /**
   * A [[Step]] that folds a stream using an initial value and an accumulation
   * function.
   *
   * @group Collection
   */
  final def fold[F[_], E, A](init: A)(f: (A, E) => A)(implicit F: Applicative[F]): Step[F, E, A] = {
    def loop(acc: A)(in: List[E]): Step[F, E, A] = in match {
      case h :: t => pureCont(loop(t.foldLeft(f(acc, h))(f)))
      case Nil => early(acc, Nil)
    }

    pureCont(loop(init))
  }

  /**
   * A [[Step]] that folds a stream using an initial value and a monadic
   * accumulation function.
   *
   * @group Collection
   */
  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] = {
    def loop(acc: A)(in: List[E]): F[Step[F, E, A]] = in match {
      case h :: t => F.map(t.foldLeft(f(acc, h))((fa, e) => F.flatMap(fa)(a => f(a, e))))(a =>
        cont(loop(a))
      )
      case Nil => F.pure(early(acc, Nil))
    }

    cont(loop(init))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a vector.
   *
   * @group Collection
   */
  final def drain[F[_], A](implicit F: Applicative[F]): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A])(in: List[A]): Step[F, A, Vector[A]] = in match {
      case Nil => early(acc, Nil)
      case els => pureCont(loop(acc ++ els))
    }

    pureCont(loop(Vector.empty))
  }

  /**
   * A [[Step]] that collects all the elements in a stream in a given collection
   * type.
   *
   * @group Collection
   */
  final def drainTo[F[_], A, C[_]](implicit
    F: Monad[F],
    M: MonoidK[C],
    C: Applicative[C]
  ): Step[F, A, C[A]] = {
    def loop(acc: C[A])(in: List[A]): Step[F, A, C[A]] = in match {
      case Nil => early(acc, Nil)
      case els => pureCont(loop(els.foldLeft(acc)((a, e) => M.combine(a, C.pure(e)))))
    }

    pureCont(loop(M.empty))
  }

  /**
   * A [[Step]] that returns the first value in a stream.
   *
   * @group Collection
   */
  final def head[F[_]: Applicative, E]: Step[F, E, Option[E]] =
    pureCont {
      case Nil => early(None, Nil)
      case h :: Nil => done(Some(h))
      case h :: t => early(Some(h), t)
    }

  /**
   * A [[Step]] that returns the first value in a stream without consuming it.
   *
   * @group Collection
   */
  final def peek[F[_]: Applicative, E]: Step[F, E, Option[E]] =
    Step.pureCont(in => early(in.headOption, in))

  /**
   * A [[Step]] that returns a given number of the first values in a stream.
   *
   * @group Collection
   */
  final def take[F[_]: Applicative, A](n: Int): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A], n: Int)(in: List[A]): Step[F, A, Vector[A]] = in match {
      case Nil => early(acc, Nil)
      case els =>
        val comp = els.lengthCompare(n)

        if (comp == 0) done(acc ++ els) else if (comp < 0) {
          val newAcc = acc ++ els
          val added = newAcc.size - acc.size

          pureCont(loop(newAcc, n - added))
        } else {
          val (taken, left) = els.splitAt(n)

          early(acc ++ taken, left)
        }
    }

    if (n <= 0) done[F, A, Vector[A]](Vector.empty) else pureCont(loop(Vector.empty, n))
  }

  /**
   * A [[Step]] that returns values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A])(in: List[A]): Step[F, A, Vector[A]] = in match {
      case Nil => early(acc, Nil)
      case h :: Nil => if (p(h)) pureCont(loop(acc :+ h)) else early(acc, in)
      case els =>
        val (before, after) = els.span(p)

        if (after.isEmpty) pureCont(loop(acc ++ before)) else early(acc ++ before, after)
    }

    pureCont(loop(Vector.empty))
  }

  /**
   * A [[Step]] that drops a given number of the values from a stream.
   *
   * @group Collection
   */
  final def drop[F[_]: Applicative, E](n: Int): Step[F, E, Unit] = {
    def loop(in: List[E]): Step[F, E, Unit] = in match {
      case Nil => early((), Nil)
      case els =>
        val size = els.size
        val comp = n - size

        if (comp == 0) done(()) else if (comp > 0) drop(comp) else early((), els.drop(n))
    }

    if (n <= 0) done(()) else pureCont(loop)
  }

  /**
   * A [[Step]] that drops values from a stream as long as they satisfy the
   * given predicate.
   *
   * @group Collection
   */
  final def dropWhile[F[_], E](p: E => Boolean)(implicit F: Applicative[F]): Step[F, E, Unit] =
    pureCont {
      case Nil => early((), Nil)
      case els =>
        val after = els.dropWhile(p)

        if (after.isEmpty) dropWhile(p) else early((), after)
    }

  /**
   * Zip two steps into a single step that returns a pair.
   *
   * @group Collection
   */
  final def zip[F[_], E, A, B](stepA: Step[F, E, A], stepB: Step[F, E, B])
    (implicit F: Monad[F]): F[Step[F, E, (A, B)]] = {
    type Pair[Z] = (Option[(Z, Option[List[E]])], Step[F, E, Z])

    def paired[Z](s: Step[F, E, Z]): Step[F, E, Pair[Z]] = Step.done(
      s.foldWith(
        new Step.Folder[F, E, Z, Pair[Z]] {
          def onCont(k: List[E] => F[Step[F, E, Z]]): Pair[Z] = (None, cont(k))
          def onDone(value: Z): Pair[Z] = (Some((value, None)), done(value))
          override def onEarly(value: Z, leftover: List[E]): Pair[Z] =
            (Some((value, Some(leftover))), early(value, leftover))
        }
      )
    )

    def shorter(a: Option[List[E]], b: Option[List[E]]): Option[List[E]] =
      (a, b) match {
        case (Some(Nil), _) => Some(Nil)
        case (_, Some(Nil)) => Some(Nil)
        case (None, _) => None
        case (_, None) => None
        case (as, bs) => if (as.size < bs.size) as else bs
      }

    def loop(stepA: Step[F, E, A], stepB: Step[F, E, B])(in: List[E]): F[Step[F, E, (A, B)]] =
      in match {
        case Nil => F.flatMap(stepA.feed(Nil))(
          _.bindF(a => F.map(stepB.feed(Nil))(_.map((a, _))))
        )
        case els => F.flatMap(stepA.feed(in))(fsA =>
            paired(fsA).bindF {
              case (pairA, nextA) =>
                F.flatMap(stepB.feed(in))(fsB =>
                  paired(fsB).bindF {
                    case (pairB, nextB) => F.pure(
                      (pairA, pairB) match {
                        case (Some((resA, remA)), Some((resB, remB))) =>
                          shorter(remA, remB) match {
                            case None => done[F, E, (A, B)]((resA, resB))
                            case Some(rem) => early[F, E, (A, B)]((resA, resB), rem)
                          }
                        case (Some((resA, _)), None) => nextB.map((resA, _))
                        case (None, Some((resB, _))) => nextA.map((_, resB))
                        case _ => cont(loop(nextA, nextB))
                      }
                    )
                  }
                )
            }
          )

      }

    paired(stepA).bindF {
      case (pairA, nextA) =>
        paired(stepB).bindF {
          case (pairB, nextB) =>
            F.pure[Step[F, E, (A, B)]](
              (pairA, pairB) match {
                case (Some((resA, remA)), Some((resB, remB))) =>
                  shorter(remA, remB) match {
                    case None => done[F, E, (A, B)]((resA, resB))
                    case Some(rem) => early[F, E, (A, B)]((resA, resB), rem)
                  }

                case (Some((resA, _)), None) => nextB.map((resA, _))
                case (None, Some((resB, _))) => nextA.map((_, resB))
                case _ => cont(loop(nextA, nextB))
              }
            )
        }
    }
  }
}
