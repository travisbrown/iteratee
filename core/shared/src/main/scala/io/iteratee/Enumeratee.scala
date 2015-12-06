package io.iteratee

import algebra.{ Eq, Monoid }
import cats.{ Applicative, Monad }

abstract class Enumeratee[F[_], O, I] extends Serializable { self =>
  type Outer[A] = Iteratee[F, O, Step[F, I, A]]

  def apply[A](step: Step[F, I, A]): Outer[A]

  final def wrap(enum: Enumerator[F, O])(implicit F: Monad[F]): Enumerator[F, I] =
    new Enumerator[F, I] {
      def apply[A](s: Step[F, I, A]): Iteratee[F, I, A] =
        Iteratee.iteratee(self(s).process(enum))
    }

  /**
   * A convenience method that lets us lift a [[Step]] into a finished [[Iteratee]].
   */
  protected final def toOuter[A](step: Step[F, I, A])(implicit F: Applicative[F]): Outer[A] =
    Iteratee.done(step, Input.empty)
}

abstract class LoopingEnumeratee[F[_]: Applicative, O, I] extends Enumeratee[F, O, I] {
  protected def loop[A](k: Input[I] => Iteratee[F, I, A]): Outer[A]

  protected final def doneOrLoop[A](step: Step[F, I, A]): Outer[A] =
    step.foldWith(
      new StepFolder[F, I, A, Outer[A]] {
        def onCont(k: Input[I] => Iteratee[F, I, A]): Outer[A] = loop(k)
        def onDone(value: A, remaining: Input[I]): Outer[A] = toOuter(step)
      }
    )

  final def apply[A](step: Step[F, I, A]): Outer[A] = doneOrLoop(step)
}

abstract class FolderEnumeratee[F[_]: Applicative, O, I] extends LoopingEnumeratee[F, O, I] {
  protected def folder[A](k: Input[I] => Iteratee[F, I, A], in: Input[O]): InputFolder[O, Outer[A]]

  protected final def loop[A](k: Input[I] => Iteratee[F, I, A]): Outer[A] =
    Iteratee.cont(stepWith(k))

  protected final def stepWith[A](k: Input[I] => Iteratee[F, I, A]): Input[O] => Outer[A] =
    in => in.foldWith(folder(k, in))
}

final object Enumeratee {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  final def map[F[_]: Monad, O, I](f: O => I): Enumeratee[F, O, I] =
    new FolderEnumeratee[F, O, I] {
      def folder[A](k: Input[I] => Iteratee[F, I, A], in: Input[O]): InputFolder[O, Outer[A]] =
        new InputFolder[O, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: O): Outer[A] = k(Input.el(f(e))).advance(doneOrLoop)
          def onChunk(es: Vector[O]): Outer[A] = k(Input.chunk(es.map(f))).advance(doneOrLoop)
          def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  final def flatMap[F[_]: Monad, O, I](f: O => Enumerator[F, I]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] lazy val monoid: Monoid[Enumerator[F, I]] = implicitly

      private[this] def loop[A](step: Step[F, I, A]): Iteratee[F, O, Step[F, I, A]] =
        step.foldWith(
          new StepFolder[F, I, A, Iteratee[F, O, Step[F, I, A]]] {
            def onCont(k: Input[I] => Iteratee[F, I, A]): Outer[A] =
              Iteratee.cont[F, O, Step[F, I, A]] {
                (_: Input[O]).map(e => f(e)).foldWith(
                  new InputFolder[Enumerator[F, I], Outer[A]] {
                    def onEmpty: Outer[A] = k(Input.empty).advance(loop)
                    def onEl(e: Enumerator[F, I]): Outer[A] = e(step).advance(loop)
                    def onChunk(es: Vector[Enumerator[F, I]]): Outer[A] =
                      monoid.combineAll(es).apply(step).advance(loop)
                    def onEnd: Outer[A] = toOuter(step)
                  }
                )
              }
            def onDone(value: A, remainder: Input[I]): Outer[A] =
              toOuter(Step.done(value, Input.empty))
          }
        )

      def apply[A](step: Step[F, I, A]): Iteratee[F, O, Step[F, I, A]] = loop(step)
    }

  final def collect[F[_]: Monad, O, I](pf: PartialFunction[O, I]): Enumeratee[F, O, I] =
    new FolderEnumeratee[F, O, I] {
      def folder[A](k: Input[I] => Iteratee[F, I, A], in: Input[O]): InputFolder[O, Outer[A]] =
        new InputFolder[O, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: O): Outer[A] =
            if (pf.isDefinedAt(e))
              k(Input.el(pf(e))).advance(doneOrLoop)
            else
              Iteratee.cont(stepWith(k))
          def onChunk(es: Vector[O]): Outer[A] = k(Input.chunk(es.collect(pf))).advance(doneOrLoop)
          def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  final def filter[F[_]: Monad, E](p: E => Boolean): Enumeratee[F, E, E] =
    new FolderEnumeratee[F, E, E] {
      def folder[A](k: Input[E] => Iteratee[F, E, A], in: Input[E]): InputFolder[E, Outer[A]] =
        new InputFolder[E, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: E): Outer[A] =
            if (p(e)) k(in).advance(doneOrLoop) else Iteratee.cont(stepWith(k))
          def onChunk(es: Vector[E]): Outer[A] = k(Input.chunk(es.filter(p))).advance(doneOrLoop)
          def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  final def sequenceI[F[_]: Monad, O, I](iteratee: Iteratee[F, O, I]): Enumeratee[F, O, I] =
    new LoopingEnumeratee[F, O, I] {
      protected def loop[A](k: Input[I] => Iteratee[F, I, A]): Outer[A] =
        Iteratee.isEnd[F, O].flatMap { isEnd =>
          if (isEnd) Iteratee.done(Step.cont(k), Input.end) else stepWith(k)
        }

      private[this] def stepWith[A](
        k: Input[I] => Iteratee[F, I, A]
      ): Outer[A] = iteratee.flatMap(a => k(Input.el(a)).advance(doneOrLoop))
    }

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  final def uniq[F[_]: Monad, E](implicit E: Eq[E]): Enumeratee[F, E, E] =
    new Enumeratee[F, E, E] {
      private[this] def stepWith[A](step: Step[F, E, A], last: Option[E]): Iteratee[F, E, A] =
        step.foldWith(
          new StepFolder[F, E, A, Iteratee[F, E, A]] {
            def onCont(k: Input[E] => Iteratee[F, E, A]): Iteratee[F, E, A] = Iteratee.cont { in =>
              in.foldWith(
                new InputFolder[E, Iteratee[F, E, A]] {
                  def onEmpty: Iteratee[F, E, A] = k(in).advance(stepWith(_, last))
                  def onEl(e: E): Iteratee[F, E, A] =
                    last match {
                      case Some(v) if E.eqv(e, v) => k(Input.empty).advance(stepWith(_, last))
                      case _ => k(in).advance(stepWith(_, Some(e)))
                    }
                  def onChunk(es: Vector[E]): Iteratee[F, E, A] =
                    if (es.isEmpty) k(in).advance(stepWith(_, last)) else {
                      val (newEs, newLast) = es.foldLeft((Vector.empty[E], last)) {
                        case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) =>
                          (acc, Some(lastValue))
                        case ((acc, _), e) => (acc :+ e, Some(e))
                      }

                      k(Input.chunk(newEs)).advance(stepWith(_, newLast))
                    }
                  def onEnd: Iteratee[F, E, A] = k(in).advance(stepWith(_, last))
                }
              )
            }
            def onDone(value: A, remainder: Input[E]): Iteratee[F, E, A] = step.pointI
          }
        )

      def apply[A](step: Step[F, E, A]): Outer[A] =
        stepWith(step, None).map(Step.done(_, Input.empty))
    }
    
  /**
   * Zip with the count of elements that have been encountered.
   */
  final def zipWithIndex[F[_]: Monad, E]: Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      type StepEl[A] = Input[(E, Long)] => Iteratee[F, (E, Long), A]

      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): Outer[A] =
        step.foldWith(
          new StepFolder[F, (E, Long), A, Outer[A]] {
            def onCont(k: Input[(E, Long)] => Iteratee[F, (E, Long), A]): Outer[A] = loop(i, k)
            def onDone(value: A, remaining: Input[(E, Long)]): Outer[A] = toOuter(step)
          }
        )

      private[this] final def loop[A](i: Long, k: StepEl[A]): Outer[A] =
        Iteratee.cont(stepWith(k, i))

      final def stepWith[A](k: StepEl[A], i: Long): (Input[E] => Outer[A]) = in =>
        in.foldWith(
          new InputFolder[E, Outer[A]] {
            def onEmpty: Outer[A] = Iteratee.cont(stepWith(k, i))
            def onEl(e: E): Outer[A] = k(Input.el((e, i))).advance(doneOrLoop(i + 1))
            def onChunk(es: Vector[E]): Outer[A] =
              k(
                Input.chunk(es.zipWithIndex.map(p => (p._1, p._2 + i)))
              ).advance(doneOrLoop(i + es.size))
            def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
          }
        )

      final def apply[A](step: Step[F, (E, Long), A]): Outer[A] = doneOrLoop(0)(step)
    }

  final def grouped[F[_]: Monad, E](n: Int): Enumeratee[F, E, Vector[E]] =
    sequenceI(Iteratee.take[F, E](n))

  final def splitOn[F[_]: Monad, E](p: E => Boolean): Enumeratee[F, E, Vector[E]] = sequenceI(
    Iteratee.takeWhile[F, E](e => !p(e)).flatMap(es => Iteratee.drop(1).map(_ => es))
  )

  final def cross[F[_]: Monad, E1, E2](e2: Enumerator[F, E2]): Enumeratee[F, E1, (E1, E2)] =
    new Enumeratee[F, E1, (E1, E2)] {
      private[this] def outerLoop[A](
        step: Step[F, (E1, E2), A]
      ): Outer[A] =
        Iteratee.head[F, E1].flatMap {
          case Some(e) => 
            val pairingIteratee = Enumeratee.map[F, E2, (E1, E2)]((e, _)).apply(step)
            val nextStep = pairingIteratee.process(e2)
            Iteratee.iteratee(nextStep).advance(outerLoop)

          case None => Iteratee.done(step, Input.end)
        }

      def apply[A](step: Step[F, (E1, E2), A]): Outer[A] = outerLoop(step)
    }
}
