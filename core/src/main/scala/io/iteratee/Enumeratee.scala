package io.iteratee

import algebra.{ Eq, Monoid }
import cats.{ Applicative, Monad }
import io.iteratee.internal.{ Input, FuncContStep, Step }

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
      final def folder[A](k: Input[I] => F[Step[F, I, A]]): Input.Folder[O, OuterF[A]] =
        new Input.Folder[O, OuterF[A]] {
          final def onEl(e: O): OuterF[A] = F.flatMap(k(Input.el(f(e))))(doneOrLoop)
          final def onChunk(e1: O, e2: O, es: Vector[O]): OuterF[A] =
            F.flatMap(k(Input.chunk(f(e1), f(e2), es.map(f))))(doneOrLoop)
          final def onEnd: OuterF[A] = F.pure(Step.early(Step.cont(k), Input.end))
        }
    }

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   */
  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        if (step.isDone) Step.done(Step.done(step.unsafeValue)) else
          new Step.Cont[F, O, Step[F, I, A]] {
            final def onEl(e: O): OuterF[A] = F.map(f(e)(step))(loop)
            final def onChunk(e1: O, e2: O, es: Vector[O]): OuterF[A] =
              F.map(
                es.foldLeft(f(e1).append(f(e2)))((acc, e) => acc.append(f(e)))(step)
              )(loop)
            final def onEnd: OuterF[A] = toOuterF(step)
          }

      final def apply[A](step: Step[F, I, A]): OuterF[A] = F.pure(loop(step))
    }

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   */
  final def collect[F[_], O, I](pf: PartialFunction[O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Folding[F, O, I] {
      final def folder[A](k: Input[I] => F[Step[F, I, A]]): Input.Folder[O, OuterF[A]] =
        new Input.Folder[O, OuterF[A]] {
          final def onEl(e: O): OuterF[A] =
            if (pf.isDefinedAt(e))
              F.flatMap(k(Input.el(pf(e))))(doneOrLoop)
            else
              F.pure(Step.cont(stepWith(k)))
          final def onChunk(e1: O, e2: O, es: Vector[O]): OuterF[A] = {
            val collected = (e1 +: e2 +: es).collect(pf)

            if (collected.isEmpty) F.pure(Step.cont(stepWith(k))) else
              if (collected.size == 1)
                F.flatMap(k(Input.el(collected(0))))(doneOrLoop)
              else
                F.flatMap(k(Input.chunk(collected(0), collected(1), collected.drop(2))))(doneOrLoop)
          }
          final def onEnd: OuterF[A] = F.pure(Step.early(Step.cont(k), Input.end))
        }
    }

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new Folding[F, E, E] {
      final def folder[A](k: Input[E] => F[Step[F, E, A]]): Input.Folder[E, OuterF[A]] =
        new Input.Folder[E, OuterF[A]] {
          final def onEl(e: E): OuterF[A] =
            if (p(e)) F.flatMap(k(Input.el(e)))(doneOrLoop) else F.pure(Step.cont(stepWith(k)))
          final def onChunk(e1: E, e2: E, es: Vector[E]): OuterF[A] = {
            val filtered = (e1 +: e2 +: es).filter(p)

            if (filtered.isEmpty) F.pure(Step.cont(stepWith(k))) else
              if (filtered.size == 1)
                F.flatMap(k(Input.el(filtered(0))))(doneOrLoop)
              else
                F.flatMap(k(Input.chunk(filtered(0), filtered(1), filtered.drop(2))))(doneOrLoop)
          }
          final def onEnd: OuterF[A] = F.pure(Step.early(Step.cont(k), Input.end))
        }
    }

  /**
   * Apply the given [[Iteratee]] repeatedly.
   */
  final def sequenceI[F[_], O, I](iteratee: Iteratee[F, O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      protected final def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A] =
        Step.cont[F, O, Boolean](in => F.pure(Step.early(in.isEnd, in))).bindF { isEnd =>
          if (isEnd) F.pure[OuterS[A]](Step.early(Step.cont(k), Input.end)) else stepWith(k)
        }

      private[this] final def stepWith[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A] =
        F.flatMap(iteratee.state)(
          _.bindF(a => F.flatMap[Step[F, I, A], OuterS[A]](k(Input.el(a)))(doneOrLoop))
        )

      protected final def doneOrLoop[A](step: Step[F, I, A]): OuterF[A] =
        if (step.isDone) F.pure(Step.done(step)) else loop(step.asFunction)

      final def apply[A](step: Step[F, I, A]): OuterF[A] = doneOrLoop(step)
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
            final def onCont(k: Input[E] => F[Step[F, E, A]]): Step[F, E, A] = Step.cont { in =>
              in.foldWith(
                new Input.Folder[E, F[Step[F, E, A]]] {
                  final def onEl(e: E): F[Step[F, E, A]] =
                    last match {
                      case Some(v) if E.eqv(e, v) => F.pure(stepWith(Step.cont(k), last))
                      case _ => F.map(k(in))(stepWith(_, Some(e)))
                    }
                  final def onChunk(e1: E, e2: E, es: Vector[E]): F[Step[F, E, A]] =
                    {
                      val (newEs, newLast) = (e1 +: e2 +: es).foldLeft((Vector.empty[E], last)) {
                        case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) =>
                          (acc, Some(lastValue))
                        case ((acc, _), e) => (acc :+ e, Some(e))
                      }

                      if (newEs.isEmpty) F.pure(stepWith(Step.cont(k), last)) else
                        if (newEs.size == 1)
                          F.map(k(Input.el(newEs(0))))(stepWith(_, newLast))
                        else
                          F.map(k(Input.chunk(newEs(0), newEs(1), newEs.drop(2))))(stepWith(_, newLast))
                    }
                  final def onEnd: F[Step[F, E, A]] = F.map(k(in))(stepWith(_, last))
                }
              )
            }
            final def onDone(value: A): Step[F, E, A] = step
          }
        )

      final def apply[A](step: Step[F, E, A]): OuterF[A] =
        F.pure(stepWith(step, None).map(Step.done[F, E, A]))
    }

  /**
   * Zip with the number of elements that have been encountered.
   */
  final def zipWithIndex[F[_], E](implicit F: Monad[F]): Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      type StepEl[A] = Input[(E, Long)] => F[Step[F, (E, Long), A]]

      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): OuterF[A] =
        step.foldWith(
          new Step.Folder[F, (E, Long), A, OuterF[A]] {
            final def onCont(k: Input[(E, Long)] => F[Step[F, (E, Long), A]]): OuterF[A] = loop(i, k)
            final def onDone(value: A): OuterF[A] = toOuterF(step)
          }
        )

      private[this] final def loop[A](i: Long, k: StepEl[A]): OuterF[A] =
        F.pure(Step.cont(stepWith(k, i)))

      final def stepWith[A](k: StepEl[A], i: Long): (Input[E] => OuterF[A]) = in =>
        in.foldWith(
          new Input.Folder[E, OuterF[A]] {
            final def onEl(e: E): OuterF[A] = F.flatMap(k(Input.el((e, i))))(doneOrLoop(i + 1))
            final def onChunk(e1: E, e2: E, es: Vector[E]): OuterF[A] =
              F.flatMap(
                k(
                  Input.chunk((e1, i), (e2, i + 1), es.zipWithIndex.map(p => (p._1, p._2 + i + 2)))
                )
              )(doneOrLoop(i + es.size + 2))
            final def onEnd: OuterF[A] = F.pure(Step.early(Step.cont(k), in))
          }
        )

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
  final def splitOn[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, Vector[E]] = sequenceI(
    Iteratee.iteratee(Step.takeWhile[F, E](e => !p(e)).bindF(es => F.pure(Step.drop(1).map(_ => es))))
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

            case None => F.pure(Step.early(step, Input.end))
          }
        )

      final def apply[A](step: Step[F, (E1, E2), A]): OuterF[A] = outerLoop(step)
    }

  abstract class Looping[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A]

    protected final def doneOrLoop[A](step: Step[F, I, A]): OuterF[A] =
      step.foldWith(
        new Step.Folder[F, I, A, OuterF[A]] {
          final def onCont(k: Input[I] => F[Step[F, I, A]]): OuterF[A] = loop(k)
          final def onDone(value: A): OuterF[A] = toOuterF(step)
        }
      )

    final def apply[A](step: Step[F, I, A]): OuterF[A] = doneOrLoop(step)
  }

  abstract class Folding[F[_], O, I](implicit F: Applicative[F]) extends Looping[F, O, I] {
    protected def folder[A](k: Input[I] => F[Step[F, I, A]]): Input.Folder[O, OuterF[A]]
    protected final def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A] = F.pure(Step.cont(stepWith(k)))
    protected final def stepWith[A](k: Input[I] => F[Step[F, I, A]]): Input[O] => OuterF[A] = _.foldWith(folder(k))
  }
}
