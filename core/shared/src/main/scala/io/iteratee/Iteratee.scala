package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Comonad, FlatMap, Functor, Id, Monad, MonadError, MonoidK, Show }
import cats.arrow.NaturalTransformation
import cats.functor.Contravariant

/**
 * An iteratee processes a stream of elements of type `E` and may produce a value of type `F[A]`.
 *
 * @see [[Step]]
 *
 * @tparam F A type constructor representing a context for effects
 * @tparam E The type of the input data
 * @tparam A The type of the calculated result
 */
sealed class Iteratee[F[_], E, A] private(final val step: F[Step[F, E, A]]) extends Serializable {
  /**
   * Collapse this iteratee to an effectful value using the provided functions.
   */
  final def foldWith[Z](folder: StepFolder[F, E, A, Z])(implicit F: Functor[F]): F[Z] =
    F.map(step)(_.foldWith(folder))

  /**
   * Run this iteratee and close the stream so that it must produce an effectful value.
   *
   * @note A well-behaved iteratee will always be in the completed state after processing an
   *       end-of-stream [[Input]] value.
   */
  final def run(implicit F: Monad[F]): F[A] = F.map(feed(Enumerator.enumEnd).step)(_.unsafeValue)

  final def flatMap[B](f: A => Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, B] = {
    def through(it: Iteratee[F, E, A]): Iteratee[F, E, B] = Iteratee.iteratee(
      F.flatMap(it.step)(
        _.foldWith(
          new StepFolder[F, E, A, F[Step[F, E, B]]] {
            def onCont(k: Input[E] => Iteratee[F, E, A]): F[Step[F, E, B]] =
              F.pure(Step.cont(u => through(k(u))))
            def onDone(value: A, remainder: Input[E]): F[Step[F, E, B]] =
              if (remainder.isEmpty) f(value).step else F.flatMap(f(value).step)(
                _.foldWith(
                  new StepFolder[F, E, B, F[Step[F, E, B]]] {
                    def onCont(ff: Input[E] => Iteratee[F, E, B]): F[Step[F, E, B]] =
                      ff(remainder).step
                    def onDone(aa: B, r: Input[E]): F[Step[F, E, B]] =
                      F.pure(Step.done(aa, remainder))
                  }
                )
              )
            }
          )
        )
      )

    through(this)
  }

  def map[B](f: A => B)(implicit F: Monad[F]): Iteratee[F, E, B] =
    flatMap(a => Step.done(f(a), Input.empty).pointI)

  def contramap[E2](f: E2 => E)(implicit F: Monad[F]): Iteratee[F, E2, A] = {
    def step(s: Step[F, E, A]): Iteratee[F, E2, A] = s.foldWith(
      new StepFolder[F, E, A, Iteratee[F, E2, A]] {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E2, A] =
          Iteratee.cont((in: Input[E2]) => k(in.map(f)).advance(step))
        def onDone(value: A, remainder: Input[E]): Iteratee[F, E2, A] =
          Iteratee.done(value, if (remainder.isEnd) Input.end else Input.empty)
      }
    )

    this.advance(step)
  }

  /**
   * Advance this iteratee using a function that transforms a step into an iteratee.
   *
   * @param f An enumerator-like function that may change the types of the input and result.
   */
  final def advance[E2, A2](f: Step[F, E, A] => Iteratee[F, E2, A2])(implicit
    F: FlatMap[F]
  ): Iteratee[F, E2, A2] = Iteratee.iteratee(F.flatMap(step)(s => f(s).step))

  /**
   * Feed an enumerator to this iteratee.
   */
  final def feed(enumerator: Enumerator[F, E])(implicit F: FlatMap[F]): Iteratee[F, E, A] =
    Iteratee.iteratee(F.flatMap(step)(s => enumerator(s).step))

  /**
   * Feed an enumerator to this iteratee and run it to return an effectful value.
   */
  final def process(enumerator: Enumerator[F, E])(implicit F: Monad[F]): F[A] = feed(enumerator).run

  /**
   * Create a new iteratee that first processes values with the given enumeratee.
   */
  final def through[O](enumeratee: Enumeratee[F, O, E])(implicit F: Monad[F]): Iteratee[F, O, A] =
    Iteratee.joinI(Iteratee.iteratee(F.flatMap(step)(s => enumeratee(s).step)))

  final def mapI[G[_]](f: NaturalTransformation[F, G])(implicit
    F: Functor[F]
  ): Iteratee[G, E, A] = {
    def transform: Step[F, E, A] => Step[G, E, A] = _.foldWith(
      new StepFolder[F, E, A, Step[G, E, A]] {
        def onCont(k: Input[E] => Iteratee[F, E, A]): Step[G, E, A] = Step.cont(in => loop(k(in)))
        def onDone(value: A, remainder: Input[E]): Step[G, E, A] = Step.done(value, remainder)
      }
    )

    def loop: Iteratee[F, E, A] => Iteratee[G, E, A] =
      i => Iteratee.iteratee(f(F.map(i.step)(transform)))

    loop(this)
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

  final def zip[B](other: Iteratee[F, E, B])(implicit F: Monad[F]): Iteratee[F, E, (A, B)] = {
    type Pair[Z] = (Option[(Z, Input[E])], Iteratee[F, E, Z])

    def step[Z](i: Iteratee[F, E, Z]): Iteratee[F, E, Pair[Z]] = Iteratee.liftM(
      i.foldWith(
        new StepFolder[F, E, Z, Pair[Z]] {
          def onCont(k: Input[E] => Iteratee[F, E, Z]): Pair[Z] = (None, Iteratee.cont(k))
          def onDone(value: Z, remainder: Input[E]): Pair[Z] =
            (Some((value, remainder)), Iteratee.done(value, remainder))
        }
      )
    )

    def feedInput[Z](i: Iteratee[F, E, Z], in: Input[E]): Iteratee[F, E, Z] = i.advance(s =>
      s.foldWith(
        new MapContStepFolder[F, E, Z](s) {
          def onCont(k: Input[E] => Iteratee[F, E, Z]): Iteratee[F, E, Z] = k(in)
        }
      )
    )

    def loop(itA: Iteratee[F, E, A], itB: Iteratee[F, E, B])(in: Input[E]): Iteratee[F, E, (A, B)] =
      in.foldWith(
        new InputFolder[E, Iteratee[F, E, (A, B)]] {
          def onEmpty: Iteratee[F, E, (A, B)] = Iteratee.cont(loop(itA, itB))
          def onEl(e: E): Iteratee[F, E, (A, B)] = step(feedInput(itA, in)).flatMap {
            case (pairA, nextItA) =>
              step(feedInput(itB, in)).flatMap {
                case (pairB, nextItB) => (pairA, pairB) match {
                  case (Some((resA, remA)), Some((resB, remB))) =>
                    Iteratee.done((resA, resB), remA.shorter(remB))
                  case (Some((resA, _)), None) => nextItB.map((resA, _))
                  case (None, Some((resB, _))) => nextItA.map((_, resB))
                  case _ => Iteratee.cont(loop(nextItA, nextItB))
                }
              }
          }

          def onChunk(es: Vector[E]): Iteratee[F, E, (A, B)] = step(feedInput(itA, in)).flatMap {
            case (pairA, nextItA) =>
              step(feedInput(itB, in)).flatMap {
                case (pairB, nextItB) => (pairA, pairB) match {
                  case (Some((resA, remA)), Some((resB, remB))) =>
                    Iteratee.done((resA, resB), remA.shorter(remB))
                  case (Some((resA, _)), None) => nextItB.map((resA, _))
                  case (None, Some((resB, _))) => nextItA.map((_, resB))
                  case _ => Iteratee.cont(loop(nextItA, nextItB))
                }
              }
          }

          def onEnd: Iteratee[F, E, (A, B)] =
            itA.feed(Enumerator.enumEnd[F, E]).flatMap(a =>
              itB.feed(Enumerator.enumEnd[F, E]).map((a, _))
            )
      }
    )

    step(this).flatMap {
      case (pairA, nextItA) =>
        step(other).flatMap {
          case (pairB, nextItB) => (pairA, pairB) match {
            case (Some((resA, remA)), Some((resB, remB))) =>
              Iteratee.done((resA, resB), remA.shorter(remB))
            case (Some((resA, _)), None) => nextItB.map((resA, _))
            case (None, Some((resB, _))) => nextItA.map((_, resB))
            case _ => Iteratee.cont(loop(nextItA, nextItB))
          }
        }
    }
  }
}

private[iteratee] sealed abstract class IterateeInstances0 {
  implicit final def iterateeMonad[F[_], E](implicit F: Monad[F]): Monad[
    ({ type L[x] = Iteratee[F, E, x] })#L
  ] = new IterateeMonad[F, E](F)
}

private[iteratee] sealed abstract class IterateeInstances extends IterateeInstances0 {
  implicit final def iterateeContravariant[F[_]: Monad, A]: Contravariant[
    ({ type L[x] = Iteratee[F, x, A] })#L
  ] = new Contravariant[({ type L[x] = Iteratee[F, x, A] })#L] {
    def contramap[E, E2](r: Iteratee[F, E, A])(f: E2 => E) = r.contramap(f)
  }

  implicit final def iterateeMonadError[F[_], T, E](implicit F: MonadError[F, T]): MonadError[
    ({ type L[x] = Iteratee[F, E, x] })#L,
    T
  ] = new IterateeMonadError[F, T, E](F)
}

object Iteratee extends IterateeInstances {
  private[iteratee] final def diverge[A]: A = sys.error("Divergent iteratee")

  /**
   * Lift an effectful value into an iteratee.
   */
  final def liftM[F[_]: Monad, E, A](fa: F[A]): Iteratee[F, E, A] =
    iteratee(Monad[F].map(fa)(a => Step.done(a, Input.empty)))

  final def iteratee[F[_], E, A](s: F[Step[F, E, A]]): Iteratee[F, E, A] = new Iteratee(s)

  final def cont[F[_]: Applicative, E, A](c: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] =
    Step.cont(c).pointI

  final def done[F[_]: Applicative, E, A](d: A, r: Input[E]): Iteratee[F, E, A] =
    Step.done(d, r).pointI

  final def identity[F[_]: Applicative, E]: Iteratee[F, E, Unit] = done[F, E, Unit]((), Input.empty)

  final def joinI[F[_]: Monad, E, I, B](it: Iteratee[F, E, Step[F, I, B]]): Iteratee[F, E, B] = {
    val IM = iterateeMonad[F, E]

    def check: Step[F, I, B] => Iteratee[F, E, B] = _.foldWith(
      new StepFolder[F, I, B, Iteratee[F, E, B]] {
        def onCont(k: Input[I] => Iteratee[F, I, B]): Iteratee[F, E, B] = k(Input.end).advance(
          s => if (s.isDone) check(s) else diverge
        )
        def onDone(value: B, remainder: Input[I]): Iteratee[F, E, B] = IM.pure(value)
      }
    )

    it.flatMap(check)
  }

  /**
   * An iteratee that consumes all of the input into something that is MonoidK and Applicative.
   */
  final def consumeIn[F[_], E, A[_]](implicit
    F: Monad[F],
    M: MonoidK[A],
    A: Applicative[A]
  ): Iteratee[F, E, A[E]] = {
    def step(in: Input[E]): Iteratee[F, E, A[E]] = in.foldWith(
      new InputFolder[E, Iteratee[F, E, A[E]]] {
        def onEmpty: Iteratee[F, E, A[E]] = cont(step)
        def onEl(e: E): Iteratee[F, E, A[E]] = cont(step).map(a => M.combine[E](A.pure(e), a))
        def onChunk(es: Vector[E]): Iteratee[F, E, A[E]] =
          cont(step).map(a =>
            es.foldRight(a)((e, acc) => (M.combine(A.pure(e), acc)))
          )
        def onEnd: Iteratee[F, E, A[E]] = done(M.empty, in)
      }      
    )

    cont(step)
  }

  final def consume[F[_]: Monad, E]: Iteratee[F, E, Vector[E]] = {
    def step(in: Input[E]): Iteratee[F, E, Vector[E]] = in.foldWith(
      new InputFolder[E, Iteratee[F, E, Vector[E]]] {
        def onEmpty: Iteratee[F, E, Vector[E]] = cont(step)
        def onEl(e: E): Iteratee[F, E, Vector[E]] = cont(step).map(e +: _)
        def onChunk(es: Vector[E]): Iteratee[F, E, Vector[E]] = cont(step).map(es ++: _)
        def onEnd: Iteratee[F, E, Vector[E]] = done(Vector.empty, in)
      }      
    )

    cont(step)
  }

  /**
   * An iteratee that consumes the head of the input.
   */
  final def head[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = {
    def step(in: Input[E]): Iteratee[F, E, Option[E]] = in.normalize.foldWith(
      new InputFolder[E, Iteratee[F, E, Option[E]]] {
        def onEmpty: Iteratee[F, E, Option[E]] = cont(step)
        def onEl(e: E): Iteratee[F, E, Option[E]] = done(Some(e), Input.empty)
        def onChunk(es: Vector[E]): Iteratee[F, E, Option[E]] =
          done(Some(es.head), Input.chunk(es.tail))
        def onEnd: Iteratee[F, E, Option[E]] = done(None, in)
      }
    )

    cont(step)
  }

  /**
   * An iteratee that returns the first element of the input.
   */
  final def peek[F[_]: Applicative, E]: Iteratee[F, E, Option[E]] = {
    def step(in: Input[E]): Iteratee[F, E, Option[E]] = in.normalize.foldWith(
      new InputFolder[E, Iteratee[F, E, Option[E]]] {
        def onEmpty: Iteratee[F, E, Option[E]] = cont(step)
        def onEl(e: E): Iteratee[F, E, Option[E]] = done(Some(e), in)
        def onChunk(es: Vector[E]): Iteratee[F, E, Option[E]] = done(Some(es.head), in)
        def onEnd: Iteratee[F, E, Option[E]] = done(None, in)
      }
    )

    cont(step)
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for `F[_]` with efficient cons, e.g. `List`.
   */
  final def reversed[F[_]: Applicative, A]: Iteratee[F, A, List[A]] =
    Iteratee.fold[F, A, List[A]](Nil)((acc, e) => e :: acc)

  /**
   * Iteratee that collects the first `n` inputs.
   */
  final def take[F[_]: Applicative, A](n: Int): Iteratee[F, A, Vector[A]] = {
    def loop(acc: Vector[A], n: Int)(in: Input[A]): Iteratee[F, A, Vector[A]] = in.foldWith(
      new InputFolder[A, Iteratee[F, A, Vector[A]]] {
        def onEmpty: Iteratee[F, A, Vector[A]] = cont(loop(acc, n))
        def onEl(e: A): Iteratee[F, A, Vector[A]] =
          if (n == 1) done(acc :+ e, Input.empty) else cont(loop(acc :+ e, n - 1))
        def onChunk(es: Vector[A]): Iteratee[F, A, Vector[A]] = {
          val diff = n - es.size

          if (diff > 0) cont(loop(acc ++ es, diff)) else
            if (diff == 0) done(acc ++ es, Input.empty) else {
              val (taken, left) = es.splitAt(n)

              done(acc ++ taken, Input.chunk(left))
            }
        }
        def onEnd: Iteratee[F, A, Vector[A]] = done(acc, in)
      }
    )

    if (n <= 0) done(Vector.empty, Input.empty) else cont(loop(Vector.empty, n))
  }

  /**
   * Iteratee that collects inputs until the input element fails a test.
   */
  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Iteratee[F, A, Vector[A]] = {
    def loop(acc: Vector[A])(in: Input[A]): Iteratee[F, A, Vector[A]] = in.foldWith(
      new InputFolder[A, Iteratee[F, A, Vector[A]]] {
        def onEmpty: Iteratee[F, A, Vector[A]] = cont(loop(acc))
        def onEl(e: A): Iteratee[F, A, Vector[A]] =
          if (p(e)) cont(loop(acc :+ e)) else done(acc, in)

        def onChunk(es: Vector[A]): Iteratee[F, A, Vector[A]] = {
          val (before, after) = es.span(p)

          if (after.isEmpty) cont(loop(acc ++ before)) else done(acc ++ before, Input.chunk(after))
        }
        def onEnd: Iteratee[F, A, Vector[A]] = done(acc, in)
      }
    )

    cont(loop(Vector.empty))
  }

  /**
   * An iteratee that skips the first `n` elements of the input.
   */
  final def drop[F[_]: Applicative, E](n: Int): Iteratee[F, E, Unit] = {
    def step(in: Input[E]): Iteratee[F, E, Unit] = in.foldWith(
      new InputFolder[E, Iteratee[F, E, Unit]] {
        def onEmpty: Iteratee[F, E, Unit] = cont(step)
        def onEl(e: E): Iteratee[F, E, Unit] = drop(n - 1)
        def onChunk(es: Vector[E]): Iteratee[F, E, Unit] = {
          val len = es.size

          if (len <= n) drop(n - len) else done((), Input.chunk(es.drop(n)))
        }
        def onEnd: Iteratee[F, E, Unit] = done((), in)
      }
    )

    if (n <= 0) done((), Input.empty) else cont(step)
  }

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  final def dropWhile[F[_]: Applicative, E](p: E => Boolean): Iteratee[F, E, Unit] = {
    def step(in: Input[E]): Iteratee[F, E, Unit] = in.foldWith(
      new InputFolder[E, Iteratee[F, E, Unit]] {
        def onEmpty: Iteratee[F, E, Unit] = cont(step)
        def onEl(e: E): Iteratee[F, E, Unit] = if (p(e)) dropWhile(p) else done((), in)
        def onChunk(es: Vector[E]): Iteratee[F, E, Unit] = {
          val after = es.dropWhile(p)

          if (after.isEmpty) dropWhile(p) else done((), Input.chunk(after))
        }
        def onEnd: Iteratee[F, E, Unit] = done((), in)
      }
    )

    cont(step)
  }

  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Iteratee[F, E, A] = {
    def step(acc: A): Input[E] => Iteratee[F, E, A] = _.foldWith(
      new InputFolder[E, Iteratee[F, E, A]] {
        def onEmpty: Iteratee[F, E, A] = cont(step(acc))
        def onEl(e: E): Iteratee[F, E, A] = cont(step(f(acc, e)))
        def onChunk(es: Vector[E]): Iteratee[F, E, A] = cont(step(es.foldLeft(acc)(f)))
        def onEnd: Iteratee[F, E, A] = done(acc, Input.end)
      }
    )

    cont(step(init))
  }

  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit
    F: Monad[F]
  ): Iteratee[F, E, A] = {
    def step(acc: A): Input[E] => Iteratee[F, E, A] = _.foldWith(
      new InputFolder[E, Iteratee[F, E, A]] {
        def onEmpty: Iteratee[F, E, A] = cont(step(acc))
        def onEl(e: E): Iteratee[F, E, A] = Iteratee.liftM(f(acc, e)).flatMap(a => cont(step(a)))
        def onChunk(es: Vector[E]): Iteratee[F, E, A] =
          Iteratee.liftM(
            es.foldLeft(F.pure(acc))((fa, e) => F.flatMap(fa)(a => f(a, e)))
          ).flatMap(a => cont(step(a)))
        def onEnd: Iteratee[F, E, A] = done(acc, Input.end)
      }
    )

    cont(step(init))
  }

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  final def length[F[_]: Applicative, E]: Iteratee[F, E, Int] = fold(0)((a, _) => a + 1)

  /**
   * An iteratee that checks if the input is EOF.
   */
  final def isEnd[F[_]: Applicative, E]: Iteratee[F, E, Boolean] = cont(in => done(in.isEnd, in))

  final def sum[F[_], E](implicit F: Monad[F], E: Monoid[E]): Iteratee[F, E, E] =
    foldM(E.empty)((a, e) => F.pure(E.combine(a, e)))
}

private class IterateeMonad[F[_], E](F: Monad[F])
  extends Monad[({ type L[x] = Iteratee[F, E, x] })#L] {
  final def pure[A](a: A): Iteratee[F, E, A] = Step.done(a, Input.empty).pointI(F)
  override final def map[A, B](fa: Iteratee[F, E, A])(f: A => B): Iteratee[F, E, B] =
    fa.map(f)(F)
  final def flatMap[A, B](fa: Iteratee[F, E, A])(f: A => Iteratee[F, E, B]): Iteratee[F, E, B] =
    fa.flatMap(f)(F)
}

private class IterateeMonadError[F[_], T, E](F: MonadError[F, T]) extends IterateeMonad[F, E](F)
  with MonadError[({ type L[x] = Iteratee[F, E, x] })#L, T] {
  final def raiseError[A](e: T): Iteratee[F, E, A] = Iteratee.liftM(F.raiseError[A](e))(F)
  final def handleErrorWith[A](fa: Iteratee[F, E, A])(
    f: T => Iteratee[F, E, A]
  ): Iteratee[F, E, A] = Iteratee.iteratee(F.handleErrorWith(fa.step)(e => f(e).step))
}
