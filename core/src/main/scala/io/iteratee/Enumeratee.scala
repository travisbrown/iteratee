package io.iteratee

import algebra.{ Eq, Monoid }
import cats.{ Applicative, Monad }
import io.iteratee.internal.Step

abstract class Enumeratee[F[_], O, I] extends Serializable { self =>
  type OuterS[A] = Step[F, O, Step[F, I, A]]
  type OuterF[A] = F[OuterS[A]]
  type OuterI[A] = Iteratee[F, O, Step[F, I, A]]

  def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]]

  final def wrap(enum: Enumerator[F, O])(implicit F: Monad[F]): Enumerator[F, I] =
    new Enumerator[F, I] {
      final def apply[A](s: Step[F, I, A]): F[Step[F, I, A]] =
        F.flatMap(self(s))(enum.runStep)
    }

  final def andThen[J](other: Enumeratee[F, I, J])(implicit F: Monad[F]): Enumeratee[F, O, J] =
    new Enumeratee[F, O, J] {
      final def apply[A](step: Step[F, J, A]): F[Step[F, O, Step[F, J, A]]] =
        F.flatMap(other(step))(next => F.flatMap(self(next))(Step.joinI(_)))
    }

  final def compose[J](other: Enumeratee[F, J, O])(implicit F: Monad[F]): Enumeratee[F, J, I] =
    other.andThen(self)

  final def map[J](f: I => J)(implicit F: Monad[F]): Enumeratee[F, O, J] =
    andThen(Enumeratee.map(f))

  final def contramap[J](f: J => O)(implicit F: Monad[F]): Enumeratee[F, J, I] =
    Enumeratee.map(f).andThen(self)

  protected final def toOuterF[A](step: Step[F, I, A])(implicit F: Applicative[F]): OuterF[A] =
    F.pure(Step.done(step))
}

final object Enumeratee extends EnumerateeInstances {
  /**
   * Map a function over a stream.
   */
  final def map[F[_], O, I](f: O => I)(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Folding[F, O, I] {
      protected final def stepWith[A](k: List[I] => F[Step[F, I, A]]): List[O] => OuterF[A] =
        _ match {
          case Nil => F.pure(Step.done(Step.cont(k), Nil))
          case els => F.flatMap(k(els.map(f)))(doneOrLoop)
        }
    }

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   */
  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] lazy val monoid: Monoid[Enumerator[F, I]] = implicitly

      private[this] def loop[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
        step.foldWith(
          new Step.Folder[F, I, A, F[Step[F, O, Step[F, I, A]]]] {
            final def onCont(k: List[I] => F[Step[F, I, A]]): OuterF[A] = F.pure(
              Step.cont[F, O, Step[F, I, A]] {
                (_: List[O]).map(e => f(e)) match {
                  case Nil => toOuterF(step)
                  case h :: Nil => F.flatMap(h(step))(loop)
                  case els => F.flatMap(monoid.combineAll(els)(step))(loop)
                }
              }
            )
            final def onDone(value: A): OuterF[A] = toOuterF(Step.done(value, Nil))
          }
        )

      final def apply[A](step: Step[F, I, A]): OuterF[A] = loop(step)
    }

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   */
  final def collect[F[_], O, I](pf: PartialFunction[O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Folding[F, O, I] {
      protected final def stepWith[A](k: List[I] => F[Step[F, I, A]]): List[O] => OuterF[A] =
        _ match {
          case Nil => F.pure(Step.done(Step.cont(k), Nil))
          case els =>
            els.collect(pf) match {
              case Nil => F.pure(Step.cont(stepWith(k)))
              case collected => F.flatMap(k(collected))(doneOrLoop)
            }
        }
    }

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new Folding[F, E, E] {
      protected final def stepWith[A](k: List[E] => F[Step[F, E, A]]): List[E] => OuterF[A] =
        _ match {
          case Nil => F.pure(Step.done(Step.cont(k), Nil))
          case els =>
            els.filter(p) match {
              case Nil => F.pure(Step.cont(stepWith(k)))
              case filtered => F.flatMap(k(filtered))(doneOrLoop)
            }
        }
    }

  /**
   * Apply the given [[Iteratee]] repeatedly.
   */
  final def sequenceI[F[_], O, I](iteratee: Iteratee[F, O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Looping[F, O, I] {
      protected final def loop[A](k: List[I] => F[Step[F, I, A]]): OuterF[A] =
        Step.cont[F, O, Boolean](in => F.pure(Step.done(in.isEmpty, in))).bindF { isEnd =>
          if (isEnd) F.pure[OuterS[A]](Step.done(Step.cont(k), Nil)) else stepWith(k)
        }

      private[this] final def stepWith[A](k: List[I] => F[Step[F, I, A]]): OuterF[A] =
        F.flatMap(iteratee.state)(
          _.bindF(a => F.flatMap[Step[F, I, A], OuterS[A]](k(List(a)))(doneOrLoop))
        )
    }

  /**
   * Collapse consecutive duplicates.
   *
   * @note Assumes that the stream is sorted.
   */
  final def uniq[F[_], E](implicit F: Applicative[F], E: Eq[E]): Enumeratee[F, E, E] =
    new Enumeratee[F, E, E] {
      private[this] final def stepWith[A](step: Step[F, E, A], last: Option[E]): Step[F, E, A] =
        step.foldWith(
          new Step.Folder[F, E, A, Step[F, E, A]] {
            final def onCont(k: List[E] => F[Step[F, E, A]]): Step[F, E, A] = Step.cont {
              case Nil => F.map(k(Nil))(stepWith(_, last))
              case h :: Nil =>
                last match {
                  case Some(v) if E.eqv(h, v) => F.pure(stepWith(Step.cont(k), last))
                  case _ => F.map(k(List(h)))(stepWith(_, Some(h)))
                }
              case els =>
                val (newEs, newLast) = els.foldLeft((Vector.empty[E], last)) {
                  case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) => (acc, Some(lastValue))
                  case ((acc, _), e) => (acc :+ e, Some(e))
                }

                if (newEs.isEmpty) F.pure(stepWith(Step.cont(k), last)) else
                  F.map(k(newEs.toList))(stepWith(_, newLast))
              }
            final def onDone(value: A): Step[F, E, A] = step
          }
        )

      final def apply[A](step: Step[F, E, A]): OuterF[A] =
        F.pure(stepWith(step, None).map(Step.done[F, E, A](_)))
    }

  /**
   * Zip with the number of elements that have been encountered.
   */
  final def zipWithIndex[F[_], E](implicit F: Monad[F]): Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      type StepEl[A] = List[(E, Long)] => F[Step[F, (E, Long), A]]

      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): OuterF[A] =
        step.foldWith(
          new Step.Folder[F, (E, Long), A, OuterF[A]] {
            final def onCont(k: List[(E, Long)] => F[Step[F, (E, Long), A]]): OuterF[A] = loop(i, k)
            final def onDone(value: A): OuterF[A] = toOuterF(step)
          }
        )

      private[this] final def loop[A](i: Long, k: StepEl[A]): OuterF[A] =
        F.pure(Step.cont(stepWith(k, i)))

      final def stepWith[A](k: StepEl[A], i: Long): (List[E] => OuterF[A]) =
        _ match {
          case Nil => F.pure(Step.done(Step.cont(k), Nil))
          case h :: Nil => F.flatMap(k(List((h, i))))(doneOrLoop(i + 1)(_))
          case els => F.flatMap(
            k(els.zipWithIndex.map(p => (p._1, p._2 + i)))
          )(doneOrLoop(i + els.size)(_))
        }

      final def apply[A](step: Step[F, (E, Long), A]): OuterF[A] = doneOrLoop(0)(step)
    }

  /**
   * Split the stream into groups of a given length.
   */
  final def grouped[F[_]: Monad, E](n: Int): Enumeratee[F, E, Vector[E]] =
    sequenceI(Iteratee.take[F, E](n))

  /**
   * Split the stream using the given predicate to identify delimiters.
   */
  final def splitOn[F[_]: Monad, E](p: E => Boolean): Enumeratee[F, E, Vector[E]] = sequenceI(
    Iteratee.takeWhile[F, E](e => !p(e)).flatMap(es => Iteratee.drop(1).map(_ => es))
  )

  /**
   * Transform a stream by taking the cross-product with the given
   * [[Enumerator]].
   */
  final def cross[F[_], E1, E2](e2: Enumerator[F, E2])(implicit F: Monad[F]): Enumeratee[F, E1, (E1, E2)] =
    new Enumeratee[F, E1, (E1, E2)] {
      private[this] final def outerLoop[A](step: Step[F, (E1, E2), A]): OuterF[A] =
        F.flatMap(Iteratee.head[F, E1].state)(
          _.bindF {
            case Some(e) =>
              val pairingIteratee = Enumeratee.map[F, E2, (E1, E2)]((e, _)).apply (step)
              val nextStep = F.flatMap(pairingIteratee)(e2.runStep)
              F.flatMap(nextStep)(outerLoop)

            case None => F.pure(Step.done(step, Nil))
          }
        )

      final def apply[A](step: Step[F, (E1, E2), A]): OuterF[A] = outerLoop(step)
    }

  abstract class Looping[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](k: List[I] => F[Step[F, I, A]]): OuterF[A]

    protected final def doneOrLoop[A](step: Step[F, I, A]): OuterF[A] =
      step.foldWith(
        new Step.Folder[F, I, A, OuterF[A]] {
          final def onCont(k: List[I] => F[Step[F, I, A]]): OuterF[A] = loop(k)
          final def onDone(value: A): OuterF[A] = toOuterF(step)
        }
      )

    final def apply[A](step: Step[F, I, A]): OuterF[A] = doneOrLoop(step)
  }

  abstract class Folding[F[_], O, I](implicit F: Applicative[F]) extends Looping[F, O, I] {
    protected final def loop[A](k: List[I] => F[Step[F, I, A]]): OuterF[A] =
      F.pure(Step.cont(stepWith(k)))
    protected def stepWith[A](k: List[I] => F[Step[F, I, A]]): List[O] => OuterF[A]
  }
}
