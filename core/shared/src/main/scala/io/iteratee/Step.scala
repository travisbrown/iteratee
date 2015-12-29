package io.iteratee

import cats.{ Applicative, Functor, Monad }

/**
 * Represents a pair of functions that can be used to reduce a [[Step]] to a value.
 *
 * Combining two "functions" into a single class allows us to save allocations.
 *
 * @tparam E The type of the input data
 * @tparam F The effect type constructor
 * @tparam A The type of the result calculated by the [[Iteratee]]
 * @tparam B The type of the result of the fold
 */
abstract class StepFolder[F[_], E, A, B] extends Serializable {
  def onCont(k: Input[E] => F[Step[F, E, A]]): B
  def onDone(value: A, remainder: Input[E]): B
}

abstract class MapContStepFolder[F[_], E, A](step: Step[F, E, A])(implicit F: Applicative[F])
  extends StepFolder[F, E, A, F[Step[F, E, A]]] {
    final def onDone(value: A, remainder: Input[E]): F[Step[F, E, A]] = F.pure(step)
  }

/**
 * Represents the current state of an [[Iteratee]].
 *
 * An [[Iteratee]] has either already calculated a result ([[Step.done]]) or is waiting for more
 * data ([[Step.cont]]).
 *
 * @tparam E The type of the input data
 * @tparam F The effect type constructor
 * @tparam A The type of the result calculated by the [[Iteratee]]
 */
sealed abstract class Step[F[_], E, A] extends Serializable {
  /**
   * The [[Iteratee]]'s result.
   *
   * In some cases we know that an iteratee has been constructed in such a way that it must be in a
   * completed state, even though that's not tracked by the type system. This method provides
   * (unsafe) access to the result for use in these situations.
   */
  private[iteratee] def unsafeValue: A

  /**
   * Reduce this [[Step]] to a value using the given pair of functions.
   */
  def foldWith[B](folder: StepFolder[F, E, A, B]): B

  def isDone: Boolean

  /**
   * Create an [[Iteratee]] with this [[Step]] as its state.
   */
  final def pointI(implicit F: Applicative[F]): Iteratee[F, E, A] = Iteratee.iteratee(F.pure(this))

  def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B]
  def into[B](f: A => Step[F, E, B])(implicit F: Monad[F]): F[Step[F, E, B]]
  def intoF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]]
  def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]]

  def zip[B](other: Step[F, E, B])(implicit F: Monad[F]): F[Step[F, E, (A, B)]] = {
    type Pair[Z] = (Option[(Z, Input[E])], Step[F, E, Z])

    def paired[Z](s: Step[F, E, Z]): Step[F, E, Pair[Z]] = Step.done(
      s.foldWith(
        new StepFolder[F, E, Z, Pair[Z]] {
          def onCont(k: Input[E] => F[Step[F, E, Z]]): Pair[Z] = (None, Step.cont(k))
          def onDone(value: Z, remainder: Input[E]): Pair[Z] = (Some((value, remainder)), Step.done(value, remainder))
        }
      ),
      Input.empty
    )

    def loop(stepA: Step[F, E, A], stepB: Step[F, E, B])(in: Input[E]): F[Step[F, E, (A, B)]] =
      in.foldWith(
        new InputFolder[E, F[Step[F, E, (A, B)]]] {
          def onEmpty: F[Step[F, E, (A, B)]] = F.pure(Step.cont(loop(stepA, stepB)))
          def onEl(e: E): F[Step[F, E, (A, B)]] = F.flatMap(stepA.feed(in))(fsA =>
            paired(fsA).intoF {
              case (pairA, nextA) =>
                F.flatMap(stepB.feed(in))(fsB =>
                  paired(fsB).intoF {
                    case (pairB, nextB) => F.pure(
                      (pairA, pairB) match {
                        case (Some((resA, remA)), Some((resB, remB))) =>
                          Step.done[F, E, (A, B)]((resA, resB), remA.shorter(remB))
                        case (Some((resA, _)), None) => nextB.map((resA, _))
                        case (None, Some((resB, _))) => nextA.map((_, resB))
                        case _ => Step.cont(loop(nextA, nextB))
                      }
                    )
                  }
                )
            }
          )

          def onChunk(es: Vector[E]): F[Step[F, E, (A, B)]] = F.flatMap(stepA.feed(in))(fsA =>
            paired(fsA).intoF {
              case (pairA, nextA) =>
                F.flatMap(stepB.feed(in))(fsB =>
                  paired(fsB).intoF {
                    case (pairB, nextB) => F.pure(
                      (pairA, pairB) match {
                        case (Some((resA, remA)), Some((resB, remB))) =>
                          Step.done[F, E, (A, B)]((resA, resB), remA.shorter(remB))
                        case (Some((resA, _)), None) => nextB.map((resA, _))
                        case (None, Some((resB, _))) => nextA.map((_, resB))
                        case _ => Step.cont(loop(nextA, nextB))
                      }
                    )
                  }
                )
            }
          )

          def onEnd: F[Step[F, E, (A, B)]] = F.flatMap(stepA.feed(Input.end))(
            _.intoF(a => F.map(stepB.feed(Input.end))(_.map((a, _))))
          )
        }
      )

    paired(this).intoF {
      case (pairA, nextA) =>
        paired(other).intoF {
          case (pairB, nextB) =>
            F.pure[Step[F, E, (A, B)]](
              (pairA, pairB) match {
                case (Some((resA, remA)), Some((resB, remB))) =>
                  Step.done((resA, resB), remA.shorter(remB))

                case (Some((resA, _)), None) => nextB.map((resA, _))
                case (None, Some((resB, _))) => nextA.map((_, resB))
                case _ => Step.cont(loop(nextA, nextB))
              }
            )
        }
    }
  }
}

final object Step extends CollectionSteps {
  /**
   * Create an incomplete state that will use the given function to process the next input.
   */
  final def cont[F[_], E, A](k: Input[E] => F[Step[F, E, A]]): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = diverge[A]
    final def isDone: Boolean = false
    final def foldWith[B](folder: StepFolder[F, E, A, B]): B = folder.onCont(k)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = cont(in =>
      F.map(k(in))(_.map(f))
    )
    final def into[B](f: A => Step[F, E, B])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(u => F.flatMap(k(u))(_.into(f)))
    )

    final def intoF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(u => F.flatMap(k(u))(_.intoF(f)))
    )
    final def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = k(in)
  }

  final def pureCont[F[_], E, A](k: Input[E] => Step[F, E, A])(implicit
    F0: Applicative[F]
  ): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = diverge[A]
    final def isDone: Boolean = false
    final def foldWith[B](folder: StepFolder[F, E, A, B]): B = folder.onCont(in => F0.pure(k(in)))
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = pureCont(in =>
      k(in).map(f)
    )
    final def into[B](f: A => Step[F, E, B])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(in => k(in).into(f))
    )
    final def intoF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] = F.pure(
      cont(in => k(in).intoF(f))
    )
    final def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(k(in))
  }

  /**
   * Create a new completed state with the given result and leftover input.
   */
  final def done[F[_], E, A](value: A, remaining: Input[E]): Step[F, E, A] = new Step[F, E, A] {
    private[iteratee] final def unsafeValue: A = value
    final def isDone: Boolean = true
    final def foldWith[B](folder: StepFolder[F, E, A, B]): B = folder.onDone(value, remaining)
    final def map[B](f: A => B)(implicit F: Functor[F]): Step[F, E, B] = done(f(value), remaining)

    final def into[B](f: A => Step[F, E, B])(implicit F: Monad[F]): F[Step[F, E, B]] =
      if (remaining.isEmpty) F.pure(f(value)) else f(value).foldWith(
        new StepFolder[F, E, B, F[Step[F, E, B]]] {
          def onCont(k: Input[E] => F[Step[F, E, B]]): F[Step[F, E, B]] = k(remaining)
          def onDone(aa: B, r: Input[E]): F[Step[F, E, B]] = F.pure(done(aa, remaining))
        }
      )

    final def intoF[B](f: A => F[Step[F, E, B]])(implicit F: Monad[F]): F[Step[F, E, B]] =
      if (remaining.isEmpty) f(value) else F.flatMap(f(value))(
        _.foldWith(
          new StepFolder[F, E, B, F[Step[F, E, B]]] {
            def onCont(k: Input[E] => F[Step[F, E, B]]): F[Step[F, E, B]] = k(remaining)
            def onDone(aa: B, r: Input[E]): F[Step[F, E, B]] = F.pure(done(aa, remaining))
          }
        )
      )

    final def feed(in: Input[E])(implicit F: Applicative[F]): F[Step[F, E, A]] = F.pure(this)
  }

  final def joinI[F[_], A, B, C](step: Step[F, A, Step[F, B, C]])(implicit
    F: Monad[F]
  ): F[Step[F, A, C]] = {
    def check: Step[F, B, C] => F[Step[F, A, C]] = _.foldWith(
      new StepFolder[F, B, C, F[Step[F, A, C]]] {
        def onCont(k: Input[B] => F[Step[F, B, C]]): F[Step[F, A, C]] = F.flatMap(k(Input.end))(
          s => if (s.isDone) check(s) else diverge
        )
        def onDone(value: C, remainder: Input[B]): F[Step[F, A, C]] =
          F.pure(Step.done(value, Input.empty))
      }
    )

    step.intoF(check)
  }

  final def liftM[F[_], E, A](fa: F[A])(implicit F: Monad[F]): F[Step[F, E, A]] =
    F.map(fa)(a => done(a, Input.empty))
}
