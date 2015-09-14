package io.iteratee

import algebra.{ Monoid, Order }
import cats.{ Applicative, Monad }

abstract class Enumeratee[O, I, F[_]] { self =>
  type Outer[A] = Iteratee[O, F, Step[I, F, A]]

  def apply[A](step: Step[I, F, A]): Outer[A]

  def wrap(enum: Enumerator[O, F])(implicit F: Monad[F]): Enumerator[I, F] =
    new Enumerator[I, F] {
      def apply[A](s: Step[I, F, A]): Iteratee[I, F, A] =
        new Iteratee((self(s).feedE(enum)).run)
    }
}

abstract class LoopingEnumeratee[O, I, F[_]: Applicative] extends Enumeratee[O, I, F] {
  protected def loop[A]: (Input[I] => Iteratee[I, F, A]) => Outer[A]

  protected def doneOrLoop[A](step: Step[I, F, A]): Outer[A] =
    step.mapContOr(loop, Iteratee.done(step, Input.empty))

  def apply[A](step: Step[I, F, A]): Outer[A] = doneOrLoop[A](step)
}

abstract class FolderEnumeratee[O, I, F[_]: Applicative] extends LoopingEnumeratee[O, I, F] {
  protected def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]]

  protected def loop[A]: (Input[I] => Iteratee[I, F, A]) => Outer[A] =
    k => Iteratee.cont[O, F, Step[I, F, A]](stepWith(k))

  protected def stepWith[A](k: Input[I] => Iteratee[I, F, A]): Input[O] => Outer[A] =
    in => in.foldWith(folder[A](k, in))
}

object Enumeratee {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  def map[O, I, F[_]: Monad](f: O => I): Enumeratee[O, I, F] =
    new FolderEnumeratee[O, I, F] {
      def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]] =
        new InputFolder[O, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: O): Outer[A] = k(Input.el(f(e))).feed(doneOrLoop)
          def onChunk(es: Seq[O]): Outer[A] = k(Input.chunk(es.map(f))).feed(doneOrLoop)
          def onEof: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  def flatMap[O, I, F[_]: Monad](f: O => Enumerator[I, F]): Enumeratee[O, I, F] =
    new Enumeratee[O, I, F] {
      private[this] lazy val monoid: Monoid[Enumerator[I, F]] = implicitly

      private[this] def loop[A](step: Step[I, F, A]): Iteratee[O, F, Step[I, F, A]] =
        step.foldWith(
          new StepFolder[I, F, A, Iteratee[O, F, Step[I, F, A]]] {
            def onCont(k: Input[I] => Iteratee[I, F, A]): Outer[A] =
              Iteratee.cont[O, F, Step[I, F, A]] {
                (_: Input[O]).map(e => f(e)).foldWith(
                  new InputFolder[Enumerator[I, F], Outer[A]] {
                    def onEmpty: Outer[A] = k(Input.empty).feed(loop)
                    def onEl(e: Enumerator[I, F]): Outer[A] = e(step).feed(loop)
                    def onChunk(es: Seq[Enumerator[I, F]]): Outer[A] =
                      monoid.combineAll(es).apply(step).feed(loop)
                    def onEof: Outer[A] = Iteratee.done(step, Input.empty)
                  }
                )
              }
            def onDone(value: A, remainder: Input[I]): Outer[A] =
              Iteratee.done(Step.done(value, Input.empty), Input.empty)
          }
        )

      def apply[A](step: Step[I, F, A]): Iteratee[O, F, Step[I, F, A]] = loop(step)
    }

  def collect[O, I, F[_]: Monad](pf: PartialFunction[O, I]): Enumeratee[O, I, F] =
    new FolderEnumeratee[O, I, F] {
      def folder[A](k: Input[I] => Iteratee[I, F, A], in: Input[O]): InputFolder[O, Outer[A]] =
        new InputFolder[O, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: O): Outer[A] =
            if (pf.isDefinedAt(e))
              k(Input.el(pf(e))).feed(doneOrLoop)
            else
              Iteratee.cont(stepWith(k))
          def onChunk(es: Seq[O]): Outer[A] = k(Input.chunk(es.collect(pf))).feed(doneOrLoop)
          def onEof: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  def filter[E, F[_]: Monad](p: E => Boolean): Enumeratee[E, E, F] =
    new FolderEnumeratee[E, E, F] {
      def folder[A](k: Input[E] => Iteratee[E, F, A], in: Input[E]): InputFolder[E, Outer[A]] =
        new InputFolder[E, Outer[A]] {
          def onEmpty: Outer[A] = Iteratee.cont(stepWith(k))
          def onEl(e: E): Outer[A] =
            if (p(e)) k(in).feed(doneOrLoop) else Iteratee.cont(stepWith(k))
          def onChunk(es: Seq[E]): Outer[A] = k(Input.chunk(es.filter(p))).feed(doneOrLoop)
          def onEof: Outer[A] = Iteratee.done(Step.cont(k), in)
        }
    }

  def sequenceI[O, I, F[_]: Monad](iteratee: Iteratee[O, F, I]): Enumeratee[O, I, F] =
    new LoopingEnumeratee[O, I, F] {
      protected def loop[A]: (Input[I] => Iteratee[I, F, A]) => Outer[A] = k =>
        Iteratee.isEof[O, F].flatMap { eof =>
          if (eof) Iteratee.done(Step.cont(k), Input.eof) else stepWith(k)
        }

      private[this] def stepWith[A](
        k: Input[I] => Iteratee[I, F, A]
      ): Outer[A] = iteratee.flatMap(a => k(Input.el(a)).feed(doneOrLoop))
    }

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  def uniq[E: Order, F[_]: Monad]: Enumeratee[E, E, F] =
    new Enumeratee[E, E, F] {
      private[this] def stepWith[A](s: Step[E, F, A], last: Input[E]): Iteratee[E, F, A] =
        s.mapCont { k => 
          Iteratee.cont { in =>
            val inr = in.filter(e => last.forall(l => Order[E].neqv(e, l)))
            k(inr).feed(stepWith(_, in))
          }
        }

      def apply[A](step: Step[E, F, A]): Outer[A] =
        stepWith(step, Input.empty).map(Step.done(_, Input.empty))
    }
    
  /**
   * Zip with the count of elements that have been encountered.
   */
  def zipWithIndex[E, F[_]: Monad]: Enumeratee[E, (E, Long), F] =
    new Enumeratee[E, (E, Long), F] {
      type StepEl[A] = Input[(E, Long)] => Iteratee[(E, Long), F, A]

      private[this] def doneOrLoop[A](i: Long)(step: Step[(E, Long), F, A]): Outer[A] =
        step.mapContOr(loop(i), Iteratee.done(step, Input.empty))

      private[this] def loop[A](i: Long): StepEl[A] => Outer[A] =
        s => Iteratee.cont(stepWith(s, i))

      def stepWith[A](k: StepEl[A], i: Long): (Input[E] => Outer[A]) = in =>
        in.foldWith(
          new InputFolder[E, Outer[A]] {
            def onEmpty: Outer[A] = Iteratee.cont(stepWith(k, i))
            def onEl(e: E): Outer[A] = k(Input.el((e, i))).feed(doneOrLoop(i + 1))
            def onChunk(es: Seq[E]): Outer[A] =
              k(
                Input.chunk(es.zipWithIndex.map(p => (p._1, p._2 + i)))
              ).feed(doneOrLoop(i + es.size))
            def onEof: Outer[A] = Iteratee.done(Step.cont(k), in)
          }
        )

      def apply[A](step: Step[(E, Long), F, A]): Outer[A] = doneOrLoop(0)(step)
    }

  def grouped[E, F[_]: Monad](n: Int): Enumeratee[E, Vector[E], F] =
    sequenceI(Iteratee.take[E, F](n))

  def splitOn[E, F[_]: Monad](p: E => Boolean): Enumeratee[E, Vector[E], F] =
    sequenceI(
      Iteratee.takeWhile[E, F](e => !p(e)).flatMap(es => Iteratee.drop(1).map(_ => es))
    )

  def cross[E1, E2, F[_]: Monad](e2: Enumerator[E2, F]): Enumeratee[E1, (E1, E2), F] =
    new Enumeratee[E1, (E1, E2), F] {
      private[this] def outerLoop[A](
        step: Step[(E1, E2), F, A]
      ): Outer[A] =
        Iteratee.head[E1, F].flatMap {
          case Some(e) => 
            val pairingIteratee = Enumeratee.map[E2, (E1, E2), F]((e, _)).apply(step)
            val nextStep = pairingIteratee.feedE(e2).run
            Iteratee.iteratee(nextStep).feed(outerLoop)

          case None => Iteratee.done(step, Input.eof[E1])
        }

      def apply[A](step: Step[(E1, E2), F, A]): Outer[A] = outerLoop(step)
    }
}
