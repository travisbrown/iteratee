package io.iteratee

import algebra.{ Monoid, Order }
import cats.{ Applicative, Monad }

abstract class Enumeratee[O, I, F[_]] extends Serializable { self =>
  type Outer[A] = Iteratee[O, F, Step[I, F, A]]

  def apply[A](step: Step[I, F, A]): Outer[A]

  final def wrap(enum: Enumerator[O, F])(implicit F: Monad[F]): Enumerator[I, F] =
    new Enumerator[I, F] {
      def apply[A](s: Step[I, F, A]): Iteratee[I, F, A] =
        Iteratee.iteratee(self(s).process(enum))
    }

  /**
   * A convenience method that lets us lift a [[Step]] into a finished [[Iteratee]].
   */
  protected final def toOuter[A](step: Step[I, F, A])(implicit F: Applicative[F]): Outer[A] =
    Iteratee.done[O, F, Step[I, F, A]](step, Input.empty)
}

abstract class LoopingEnumeratee[O, I, F[_]: Applicative] extends Enumeratee[O, I, F] {
  protected def loop[A](k: Input[I] => Iteratee[I, F, A]): Outer[A]

  protected final def doneOrLoop[A](step: Step[I, F, A]): Outer[A] =
    step.foldWith(
      new StepFolder[I, F, A, Outer[A]] {
        def onCont(k: Input[I] => Iteratee[I, F, A]): Outer[A] = loop(k)
        def onDone(value: A, remaining: Input[I]): Outer[A] = toOuter(step)
      }
    )

  final def apply[A](step: Step[I, F, A]): Outer[A] = doneOrLoop[A](step)
}

abstract class FolderEnumeratee[O, I, F[_]: Applicative] extends LoopingEnumeratee[O, I, F] {
  protected def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]]

  protected final def loop[A](k: Input[I] => Iteratee[I, F, A]): Outer[A] =
    Iteratee.cont[O, F, Step[I, F, A]](stepWith(k))

  protected final def stepWith[A](k: Input[I] => Iteratee[I, F, A]): Input[O] => Outer[A] =
    in => in.foldWith(folder[A](k, in))
}

final object Enumeratee {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  final def map[O, I, F[_]: Monad](f: O => I): Enumeratee[O, I, F] =
    new FolderEnumeratee[O, I, F] {
      def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]] =
        new InputFolder[O, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: O): Outer[A] = k(Input.el(f(e))).advance(doneOrLoop)
          def onChunk(es: Vector[O]): Outer[A] = k(Input.chunk(es.map(f))).advance(doneOrLoop)
          def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  final def flatMap[O, I, F[_]: Monad](f: O => Enumerator[I, F]): Enumeratee[O, I, F] =
    new Enumeratee[O, I, F] {
      private[this] lazy val monoid: Monoid[Enumerator[I, F]] = implicitly

      private[this] def loop[A](step: Step[I, F, A]): Iteratee[O, F, Step[I, F, A]] =
        step.foldWith(
          new StepFolder[I, F, A, Iteratee[O, F, Step[I, F, A]]] {
            def onCont(k: Input[I] => Iteratee[I, F, A]): Outer[A] =
              Iteratee.cont[O, F, Step[I, F, A]] {
                (_: Input[O]).map(e => f(e)).foldWith(
                  new InputFolder[Enumerator[I, F], Outer[A]] {
                    def onEmpty: Outer[A] = k(Input.empty).advance(loop)
                    def onEl(e: Enumerator[I, F]): Outer[A] = e(step).advance(loop)
                    def onChunk(es: Vector[Enumerator[I, F]]): Outer[A] =
                      monoid.combineAll(es).apply(step).advance(loop)
                    def onEnd: Outer[A] = toOuter(step)
                  }
                )
              }
            def onDone(value: A, remainder: Input[I]): Outer[A] =
              toOuter(Step.done(value, Input.empty))
          }
        )

      def apply[A](step: Step[I, F, A]): Iteratee[O, F, Step[I, F, A]] = loop(step)
    }

  final def collect[O, I, F[_]: Monad](pf: PartialFunction[O, I]): Enumeratee[O, I, F] =
    new FolderEnumeratee[O, I, F] {
      def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]] =
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

  final def filter[E, F[_]: Monad](p: E => Boolean): Enumeratee[E, E, F] =
    new FolderEnumeratee[E, E, F] {
      def folder[A](k: Input[E] => Iteratee[E, F, A], in: Input[E]): InputFolder[E, Outer[A]] =
        new InputFolder[E, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: E): Outer[A] =
            if (p(e)) k(in).advance(doneOrLoop) else Iteratee.cont(stepWith(k))
          def onChunk(es: Vector[E]): Outer[A] = k(Input.chunk(es.filter(p))).advance(doneOrLoop)
          def onEnd: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  final def sequenceI[O, I, F[_]: Monad](iteratee: Iteratee[O, F, I]): Enumeratee[O, I, F] =
    new LoopingEnumeratee[O, I, F] {
      protected def loop[A](k: Input[I] => Iteratee[I, F, A]): Outer[A] =
        Iteratee.isEnd[O, F].flatMap { isEnd =>
          if (isEnd) Iteratee.done(Step.cont(k), Input.end) else stepWith(k)
        }

      private[this] def stepWith[A](
        k: Input[I] => Iteratee[I, F, A]
      ): Outer[A] = iteratee.flatMap(a => k(Input.el(a)).advance(doneOrLoop))
    }

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  final def uniq[E: Order, F[_]: Monad]: Enumeratee[E, E, F] =
    new Enumeratee[E, E, F] {
      private[this] def stepWith[A](step: Step[E, F, A], last: Input[E]): Iteratee[E, F, A] =
        step.foldWith(
          new StepFolder[E, F, A, Iteratee[E, F, A]] {
            def onCont(k: Input[E] => Iteratee[E, F, A]): Iteratee[E, F, A] = Iteratee.cont { in =>
              val inr = in.filter(e => last.forall(l => Order[E].neqv(e, l)))
              k(inr).advance(stepWith(_, in))
            }
            def onDone(value: A, remainder: Input[E]): Iteratee[E, F, A] = step.pointI
          }
        )

      def apply[A](step: Step[E, F, A]): Outer[A] =
        stepWith(step, Input.empty).map(Step.done(_, Input.empty))
    }
    
  /**
   * Zip with the count of elements that have been encountered.
   */
  final def zipWithIndex[E, F[_]: Monad]: Enumeratee[E, (E, Long), F] =
    new Enumeratee[E, (E, Long), F] {
      type StepEl[A] = Input[(E, Long)] => Iteratee[(E, Long), F, A]

      private[this] final def doneOrLoop[A](i: Long)(step: Step[(E, Long), F, A]): Outer[A] =
        step.foldWith(
          new StepFolder[(E, Long), F, A, Outer[A]] {
            def onCont(k: Input[(E, Long)] => Iteratee[(E, Long), F, A]): Outer[A] = loop(i, k)
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

      final def apply[A](step: Step[(E, Long), F, A]): Outer[A] = doneOrLoop(0)(step)
    }

  final def grouped[E, F[_]: Monad](n: Int): Enumeratee[E, Vector[E], F] =
    sequenceI(Iteratee.take[E, F](n))

  final def splitOn[E, F[_]: Monad](p: E => Boolean): Enumeratee[E, Vector[E], F] = sequenceI(
    Iteratee.takeWhile[E, F](e => !p(e)).flatMap(es => Iteratee.drop(1).map(_ => es))
  )

  final def cross[E1, E2, F[_]: Monad](e2: Enumerator[E2, F]): Enumeratee[E1, (E1, E2), F] =
    new Enumeratee[E1, (E1, E2), F] {
      private[this] def outerLoop[A](
        step: Step[(E1, E2), F, A]
      ): Outer[A] =
        Iteratee.head[E1, F].flatMap {
          case Some(e) => 
            val pairingIteratee = Enumeratee.map[E2, (E1, E2), F]((e, _)).apply(step)
            val nextStep = pairingIteratee.process(e2)
            Iteratee.iteratee(nextStep).advance(outerLoop)

          case None => Iteratee.done(step, Input.end[E1])
        }

      def apply[A](step: Step[(E1, E2), F, A]): Outer[A] = outerLoop(step)
    }
}
