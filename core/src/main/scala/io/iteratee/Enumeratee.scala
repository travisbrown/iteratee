package io.iteratee

import algebra.{ Eq, Monoid }
import cats.{ Applicative, Monad }
import cats.data.NonEmptyVector
import io.iteratee.internal.{ Input, Step }

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
    Enumeratee.map(f)(F).andThen(self)

  protected final def toOuterF[A](step: Step[F, I, A])(implicit F: Applicative[F]): OuterF[A] =
    F.pure(Step.done(step))
}

final object Enumeratee extends EnumerateeInstances {
  /**
   * Map a function over a stream.
   */
  final def map[F[_], O, I](f: O => I)(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Folding[F, O, I] {
      final def stepWith[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        new Step.Cont[F, O, Step[F, I, A]] {
          final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, O, Step[F, I, A]]] = F.pure(Step.ended(step))
          final def onEl(e: O): OuterF[A] = F.flatMap(step.feed(Input.el(f(e))))(doneOrLoop)
          final def onChunk(h: O, t: NonEmptyVector[O]): OuterF[A] =
            F.flatMap(step.feed(Input.chunk(f(h), NonEmptyVector(f(t.head), t.tail.map(f)))))(doneOrLoop)
        }
    }

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   */
  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        if (step.isDone) Step.done(step) else
          new Step.Cont[F, O, Step[F, I, A]] {
            final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, O, Step[F, I, A]]] = F.pure(Step.ended(step))
            final def onEl(e: O): OuterF[A] = F.map(f(e)(step))(loop)
            final def onChunk(h: O, t: NonEmptyVector[O]): OuterF[A] =
              F.map(t.tail.foldLeft(f(h).append(f(t.head)))((acc, e) => acc.append(f(e)))(step))(loop)
          }

      final def apply[A](step: Step[F, I, A]): OuterF[A] = F.pure(loop(step))
    }

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   */
  final def collect[F[_], O, I](pf: PartialFunction[O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Folding[F, O, I] {
      final def stepWith[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        new Step.Cont[F, O, Step[F, I, A]] {
          final def onEl(e: O): OuterF[A] =
            if (pf.isDefinedAt(e))
              F.flatMap(step.feed(Input.el(pf(e))))(doneOrLoop)
            else
              F.pure(stepWith(step))
          final def onChunk(h: O, t: NonEmptyVector[O]): OuterF[A] = {
            val collected = (h +: t.head +: t.tail).collect(pf)

            if (collected.isEmpty) F.pure(stepWith(step)) else
              F.flatMap(step.feed(Input.fromVectorUnsafe(collected)))(doneOrLoop)
          }
          final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, O, Step[F, I, A]]] =
            F.pure(Step.ended(step))
        }
    }

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new Folding[F, E, E] {
      final def stepWith[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
        new Step.Cont[F, E, Step[F, E, A]] {
          final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E, Step[F, E, A]]] =
            F.pure(Step.ended[F, E, Step[F, E, A]](step))
          final def onEl(e: E): OuterF[A] =
            if (p(e)) F.flatMap(step.feed(Input.el(e)))(doneOrLoop) else F.pure(stepWith(step))
          final def onChunk(h: E, t: NonEmptyVector[E]): OuterF[A] = {
            val filtered = (h +: t.head +: t.tail).filter(p)

            if (filtered.isEmpty) F.pure(stepWith(step)) else
              F.flatMap(step.feed(Input.fromVectorUnsafe(filtered)))(doneOrLoop)
          }
        }
    }

  /**
   * Apply the given [[Iteratee]] repeatedly.
   */
  final def sequenceI[F[_], O, I](iteratee: Iteratee[F, O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Looping[F, O, I] {
      protected final def loop[A](step: Step[F, I, A]): OuterF[A] = {
        Step.isEnd[F, O].bind { isEnd =>
          if (isEnd) F.pure(Step.ended(step)) else F.flatMap(iteratee.state)(
            _.bind(a => F.flatMap(step.feed(Input.el(a)))(doneOrLoop))
          )
        }
      }
    }

  /**
   * Collapse consecutive duplicates.
   *
   * @note Assumes that the stream is sorted.
   */
  final def uniq[F[_], E](implicit F: Applicative[F], E: Eq[E]): Enumeratee[F, E, E] =
    new Enumeratee[F, E, E] {
      private[this] final def stepWith[A](step: Step[F, E, A], last: Option[E]): Step[F, E, A] =
        if (step.isDone) step else new Step.Cont[F, E, A] {
          final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E, A]] = step.onEnd(F0)
          final def onEl(e: E): F[Step[F, E, A]] = last match {
            case Some(v) if E.eqv(e, v) => F.pure(stepWith(step, last))
            case _ => F.map(step.feed(Input.el(e)))(stepWith(_, Some(e)))
          }
          final def onChunk(h: E, t: NonEmptyVector[E]): F[Step[F, E, A]] = {
            val (newEs, newLast) = (h +: t.head +: t.tail).foldLeft((Vector.empty[E], last)) {
              case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) => (acc, Some(lastValue))
              case ((acc, _), e) => (acc :+ e, Some(e))
            }

            if (newEs.isEmpty) F.pure(stepWith(step, last)) else
              F.map(step.feed(Input.fromVectorUnsafe(newEs)))(stepWith(_, newLast))
          }
        }

      final def apply[A](step: Step[F, E, A]): OuterF[A] =
        F.pure(stepWith(step, None).map(Step.done[F, E, A]))
    }

  /**
   * Zip with the number of elements that have been encountered.
   */
  final def zipWithIndex[F[_], E](implicit F: Applicative[F]): Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): Step[F, E, Step[F, (E, Long), A]] =
        if (step.isDone) Step.done(step) else stepWith(i, step)

      private[this] final def stepWith[A](i: Long, step: Step[F, (E, Long), A]): Step[F, E, Step[F, (E, Long), A]] =
        new Step.Cont[F, E, Step[F, (E, Long), A]] {
          final def onEnd(implicit F0: Applicative[F]): F[Step.Ended[F, E, Step[F, (E, Long), A]]] = F.pure(
            Step.ended[F, E, Step[F, (E, Long), A]](step)
          )
          final def onEl(e: E): OuterF[A] = F.map(step.feed(Input.el((e, i))))(doneOrLoop(i + 1))
          final def onChunk(h: E, t: NonEmptyVector[E]): OuterF[A] = 
            F.map(
              step.feed(
                Input.chunk(
                  (h, i),
                  NonEmptyVector((t.head, i + 1L), t.tail.zipWithIndex.map(p => (p._1, p._2 + i + 2L)))
                )
              )
            )(doneOrLoop(i + t.tail.size + 2))
        }

      final def apply[A](step: Step[F, (E, Long), A]): OuterF[A] = F.pure(doneOrLoop(0)(step))
    }

  /**
   * Split the stream into groups of a given length.
   */
  final def grouped[F[_]: Monad, E](n: Int): Enumeratee[F, E, Vector[E]] =
    sequenceI(Iteratee.take[F, E](n))

  /**
   * Split the stream using the given predicate to identify delimiters.
   */
  final def splitOn[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, Vector[E]] = sequenceI(
    Iteratee.iteratee(Step.takeWhile[F, E](e => !p(e)).bind(es => F.pure(Step.drop[F, E](1).map(_ => es))))
  )

  /**
   * Transform a stream by taking the cross-product with the given
   * [[Enumerator]].
   */
  final def cross[F[_], E1, E2](e2: Enumerator[F, E2])(implicit F: Monad[F]): Enumeratee[F, E1, (E1, E2)] =
    new Enumeratee[F, E1, (E1, E2)] {
      private[this] final def outerLoop[A](step: Step[F, (E1, E2), A]): OuterF[A] =
        F.flatMap(Iteratee.head[F, E1].state)(
          _.bind {
            case Some(e) =>
              val pairingIteratee = Enumeratee.map[F, E2, (E1, E2)]((e, _)).apply (step)
              val nextStep = F.flatMap(pairingIteratee)(e2.runStep)
              F.flatMap(nextStep)(outerLoop)

            case None => F.pure(Step.ended(step))
          }
        )

      final def apply[A](step: Step[F, (E1, E2), A]): OuterF[A] = outerLoop(step)
    }

  abstract class Looping[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](step: Step[F, I, A]): OuterF[A]

    protected final def doneOrLoop[A](step: Step[F, I, A]): OuterF[A] =
      if (step.isDone) F.pure(Step.done(step)) else loop(step)

    final def apply[A](step: Step[F, I, A]): OuterF[A] = doneOrLoop(step)
  }

  abstract class Folding[F[_], O, I](implicit F: Applicative[F]) extends Looping[F, O, I] {
    protected final def loop[A](step: Step[F, I, A]): OuterF[A] = F.pure(stepWith(step))
    protected def stepWith[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]]
  }
}
