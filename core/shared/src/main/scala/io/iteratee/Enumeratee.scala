package io.iteratee

import algebra.{ Eq, Monoid }
import cats.{ Applicative, Monad }
import io.iteratee.internal.{ Input, InputFolder, Step, StepFolder }

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
      def apply[A](step: Step[F, J, A]): F[Step[F, O, Step[F, J, A]]] =
        F.flatMap(other(step))(next => F.flatMap(self(next))(Step.joinI(_)))
    }

  final def compose[J](other: Enumeratee[F, J, O])(implicit F: Monad[F]): Enumeratee[F, J, I] =
    other.andThen(self)

  final def map[J](f: I => J)(implicit F: Monad[F]): Enumeratee[F, O, J] =
    andThen(Enumeratee.map(f))

  final def contramap[J](f: J => O)(implicit F: Monad[F]): Enumeratee[F, J, I] =
    Enumeratee.map(f).andThen(self)

  protected final def toOuterF[A](step: Step[F, I, A])(implicit F: Applicative[F]): OuterF[A] =
    F.pure(Step.done(step, Input.empty))
}

abstract class LoopingEnumeratee[F[_], O, I](implicit F: Applicative[F])
  extends Enumeratee[F, O, I] {
  protected def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A]

  protected final def doneOrLoop[A](step: Step[F, I, A]): OuterF[A] =
    step.foldWith(
      new StepFolder[F, I, A, OuterF[A]] {
        def onCont(k: Input[I] => F[Step[F, I, A]]): OuterF[A] = loop(k)
        def onDone(value: A, remaining: Input[I]): OuterF[A] = toOuterF(step)
      }
    )

  final def apply[A](step: Step[F, I, A]): OuterF[A] = doneOrLoop(step)
}

abstract class FolderEnumeratee[F[_], O, I](implicit F: Applicative[F])
  extends LoopingEnumeratee[F, O, I] {
  protected def folder[A](k: Input[I] => F[Step[F, I, A]]): InputFolder[O, OuterF[A]]

  protected final def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A] =
    F.pure(Step.cont(stepWith(k)))

  protected final def stepWith[A](k: Input[I] => F[Step[F, I, A]]): Input[O] => OuterF[A] =
    _.foldWith(folder(k))
}

final object Enumeratee extends EnumerateeInstances {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  final def map[F[_], O, I](f: O => I)(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new FolderEnumeratee[F, O, I] {
      def folder[A](k: Input[I] => F[Step[F, I, A]]): InputFolder[O, OuterF[A]] =
        new InputFolder[O, OuterF[A]] {
          def onEmpty: OuterF[A] = F.pure(Step.cont(stepWith(k)))
          def onEl(e: O): OuterF[A] = F.flatMap(k(Input.el(f(e))))(doneOrLoop)
          def onChunk(es: Vector[O]): OuterF[A] = F.flatMap(k(Input.chunk(es.map(f))))(doneOrLoop)
          def onEnd: OuterF[A] = F.pure(Step.done(Step.cont(k), Input.end))
        }
    }

  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit
    F: Monad[F]
  ): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] lazy val monoid: Monoid[Enumerator[F, I]] = implicitly

      private[this] def loop[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
        step.foldWith(
          new StepFolder[F, I, A, F[Step[F, O, Step[F, I, A]]]] {
            def onCont(k: Input[I] => F[Step[F, I, A]]): OuterF[A] = F.pure(
              Step.cont[F, O, Step[F, I, A]] {
                (_: Input[O]).map(e => f(e)).foldWith(
                  new InputFolder[Enumerator[F, I], OuterF[A]] {
                    def onEmpty: OuterF[A] = F.flatMap(k(Input.empty))(loop)
                    def onEl(e: Enumerator[F, I]): OuterF[A] = F.flatMap(e(step))(loop)
                    def onChunk(es: Vector[Enumerator[F, I]]): OuterF[A] =
                      F.flatMap(monoid.combineAll(es)(step))(loop)
                    def onEnd: OuterF[A] = toOuterF(step)
                  }
                )
              }
            )
            def onDone(value: A, remainder: Input[I]): OuterF[A] =
              toOuterF(Step.done(value, Input.empty))
          }
        )

      def apply[A](step: Step[F, I, A]): OuterF[A] = loop(step)
    }

  final def collect[F[_], O, I](pf: PartialFunction[O, I])(implicit
    F: Monad[F]
  ): Enumeratee[F, O, I] =
    new FolderEnumeratee[F, O, I] {
      def folder[A](k: Input[I] => F[Step[F, I, A]]): InputFolder[O, OuterF[A]] =
        new InputFolder[O, OuterF[A]] {
          def onEmpty: OuterF[A] = F.pure(Step.cont(stepWith(k)))
          def onEl(e: O): OuterF[A] =
            if (pf.isDefinedAt(e))
              F.flatMap(k(Input.el(pf(e))))(doneOrLoop)
            else
              F.pure(Step.cont(stepWith(k)))
          def onChunk(es: Vector[O]): OuterF[A] =
            F.flatMap(k(Input.chunk(es.collect(pf))))(doneOrLoop)
          def onEnd: OuterF[A] = F.pure(Step.done(Step.cont(k), Input.end))
        }
    }

  final def filter[F[_], E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new FolderEnumeratee[F, E, E] {
      def folder[A](k: Input[E] => F[Step[F, E, A]]): InputFolder[E, OuterF[A]] =
        new InputFolder[E, OuterF[A]] {
          def onEmpty: OuterF[A] = F.pure(Step.cont(stepWith(k)))
          def onEl(e: E): OuterF[A] =
            if (p(e)) F.flatMap(k(Input.el(e)))(doneOrLoop) else F.pure(Step.cont(stepWith(k)))
          def onChunk(es: Vector[E]): OuterF[A] =
            F.flatMap(k(Input.chunk(es.filter(p))))(doneOrLoop)
          def onEnd: OuterF[A] = F.pure(Step.done(Step.cont(k), Input.end))
        }
    }

  final def sequenceI[F[_], O, I](iteratee: Iteratee[F, O, I])(implicit
    F: Monad[F]
  ): Enumeratee[F, O, I] =
    new LoopingEnumeratee[F, O, I] {
      protected def loop[A](k: Input[I] => F[Step[F, I, A]]): OuterF[A] =
        Step.cont[F, O, Boolean](in => F.pure(Step.done(in.isEnd, in))).intoF { isEnd =>
          if (isEnd) F.pure[OuterS[A]](Step.done(Step.cont(k), Input.end)) else stepWith(k)
        }

      private[this] def stepWith[A](
        k: Input[I] => F[Step[F, I, A]]
      ): OuterF[A] =
        F.flatMap(iteratee.state)(
          _.intoF(a => F.flatMap[Step[F, I, A], OuterS[A]](k(Input.el(a)))(doneOrLoop))
        )
    }

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  final def uniq[F[_], E](implicit F: Applicative[F], E: Eq[E]): Enumeratee[F, E, E] =
    new Enumeratee[F, E, E] {
      private[this] def stepWith[A](step: Step[F, E, A], last: Option[E]): Step[F, E, A] =
        step.foldWith(
          new StepFolder[F, E, A, Step[F, E, A]] {
            def onCont(k: Input[E] => F[Step[F, E, A]]): Step[F, E, A] = Step.cont { in =>
              in.foldWith(
                new InputFolder[E, F[Step[F, E, A]]] {
                  def onEmpty: F[Step[F, E, A]] = F.map(k(in))(stepWith(_, last))
                  def onEl(e: E): F[Step[F, E, A]] =
                    last match {
                      case Some(v) if E.eqv(e, v) => F.map(k(Input.empty))(stepWith(_, last))
                      case _ => F.map(k(in))(stepWith(_, Some(e)))
                    }
                  def onChunk(es: Vector[E]): F[Step[F, E, A]] =
                    if (es.isEmpty) F.map(k(in))(stepWith(_, last)) else {
                      val (newEs, newLast) = es.foldLeft((Vector.empty[E], last)) {
                        case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) =>
                          (acc, Some(lastValue))
                        case ((acc, _), e) => (acc :+ e, Some(e))
                      }
                      F.map(k(Input.chunk(newEs)))(stepWith(_, newLast))
                    }
                  def onEnd: F[Step[F, E, A]] = F.map(k(in))(stepWith(_, last))
                }
              )
            }
            def onDone(value: A, remainder: Input[E]): Step[F, E, A] = step
          }
        )

      def apply[A](step: Step[F, E, A]): OuterF[A] =
        F.pure(stepWith(step, None).map(Step.done[F, E, A](_, Input.empty)))
    }
    
  /**
   * Zip with the count of elements that have been encountered.
   */
  final def zipWithIndex[F[_], E](implicit F: Monad[F]): Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      type StepEl[A] = Input[(E, Long)] => F[Step[F, (E, Long), A]]

      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): OuterF[A] =
        step.foldWith(
          new StepFolder[F, (E, Long), A, OuterF[A]] {
            def onCont(k: Input[(E, Long)] => F[Step[F, (E, Long), A]]): OuterF[A] = loop(i, k)
            def onDone(value: A, remaining: Input[(E, Long)]): OuterF[A] = toOuterF(step)
          }
        )

      private[this] final def loop[A](i: Long, k: StepEl[A]): OuterF[A] =
        F.pure(Step.cont(stepWith(k, i)))

      final def stepWith[A](k: StepEl[A], i: Long): (Input[E] => OuterF[A]) = in =>
        in.foldWith(
          new InputFolder[E, OuterF[A]] {
            def onEmpty: OuterF[A] = F.pure(Step.cont(stepWith(k, i)))
            def onEl(e: E): OuterF[A] = F.flatMap(k(Input.el((e, i))))(doneOrLoop(i + 1))
            def onChunk(es: Vector[E]): OuterF[A] =
              F.flatMap(
                k(
                  Input.chunk(es.zipWithIndex.map(p => (p._1, p._2 + i)))
                )
              )(doneOrLoop(i + es.size))
            def onEnd: OuterF[A] = F.pure(Step.done(Step.cont(k), in))
          }
        )

      final def apply[A](step: Step[F, (E, Long), A]): OuterF[A] = doneOrLoop(0)(step)
    }

  final def grouped[F[_]: Monad, E](n: Int): Enumeratee[F, E, Vector[E]] =
    sequenceI(Iteratee.take[F, E](n))

  final def splitOn[F[_]: Monad, E](p: E => Boolean): Enumeratee[F, E, Vector[E]] = sequenceI(
    Iteratee.takeWhile[F, E](e => !p(e)).flatMap(es => Iteratee.drop(1).map(_ => es))
  )

  final def cross[F[_], E1, E2](e2: Enumerator[F, E2])(implicit
    F: Monad[F]
  ): Enumeratee[F, E1, (E1, E2)] =
    new Enumeratee[F, E1, (E1, E2)] {
      private[this] def outerLoop[A](
        step: Step[F, (E1, E2), A]
      ): OuterF[A] =
        F.flatMap(Iteratee.head[F, E1].state)(
          _.intoF {
            case Some(e) => 
              val pairingIteratee = Enumeratee.map[F, E2, (E1, E2)]((e, _)).apply (step)
              val nextStep = F.flatMap(pairingIteratee)(e2.runStep)
              F.flatMap(nextStep)(outerLoop)

            case None => F.pure(Step.done(step, Input.end))
          }
        )

      def apply[A](step: Step[F, (E1, E2), A]): OuterF[A] = outerLoop(step)
    }
}
