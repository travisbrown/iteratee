package io.iteratee

import cats.{ Applicative, Eq, FlatMap, Monad }
import cats.instances.list.catsStdInstancesForList
import io.iteratee.internal.Step
import scala.collection.mutable.Builder

abstract class Enumeratee[F[_], O, I] extends Serializable { self =>
  def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]]

  final def wrap(enum: Enumerator[F, O])(implicit F: FlatMap[F]): Enumerator[F, I] = new Enumerator[F, I] {
    final def apply[A](s: Step[F, I, A]): F[Step[F, I, A]] = F.flatMap(self(s))(enum.intoStep)
  }

  final def into[A](iteratee: Iteratee[F, I, A])(implicit F: Monad[F]): Iteratee[F, O, A] = iteratee.through(this)

  final def andThen[J](other: Enumeratee[F, I, J])(implicit F: Monad[F]): Enumeratee[F, O, J] = other.compose(self)

  final def compose[J](other: Enumeratee[F, J, O])(implicit F: Monad[F]): Enumeratee[F, J, I] =
    new Enumeratee[F, J, I] {
      final def apply[A](step: Step[F, I, A]): F[Step[F, J, Step[F, I, A]]] =
        F.flatMap(self(step))(next => F.flatMap(other(next))(Step.joinI(_)))
    }

  final def map[J](f: I => J)(implicit F: Monad[F]): Enumeratee[F, O, J] = andThen(Enumeratee.map(f))

  final def contramap[J](f: J => O)(implicit F: Monad[F]): Enumeratee[F, J, I] = Enumeratee.map(f)(F).andThen(self)
}

final object Enumeratee extends EnumerateeInstances {
  /**
   * An identity stream transformer.
   */
  final def identity[F[_], E](implicit F: Applicative[F]): Enumeratee[F, E, E] = new PureLoop[F, E, E] {
    protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new IdentityCont(step)
  }

  /**
   * Map a function over a stream.
   */
  final def map[F[_], O, I](f: O => I)(implicit F: Applicative[F]): Enumeratee[F, O, I] = new PureLoop[F, O, I] {
    protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new StepCont[F, O, I, A](step) {
      final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(step.feedEl(f(e)))(doneOrLoop)
      final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] =
        F.map(step.feed(chunk.map(f)))(doneOrLoop)
    }
  }

  /**
   * Map a function returning a value in a context over a stream.
   */
  final def flatMapM[F[_], O, I](f: O => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] = new PureLoop[F, O, I] {
    protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new StepCont[F, O, I, A](step) {
      final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]]= F.map(F.flatMap(f(e))(step.feedEl))(doneOrLoop)
      final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] = F.map(
        F.flatMap(catsStdInstancesForList.traverse(chunk.toList)(f))(step.feed)
      )(doneOrLoop)
    }
  }

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   */
  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new PureLoop[F, O, I] {
      protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new StepCont[F, O, I, A](step) {
        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(f(e)(step))(doneOrLoop)
        final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] =
          F.map(chunk.tail.foldLeft(f(chunk.head))((acc, e) => acc.append(f(e))).apply(step))(doneOrLoop)
      }
    }

  /**
   * An [[Enumeratee]] that takes a given number of the first values in a
   * stream.
   */
  final def take[F[_], E](n: Long)(implicit F: Applicative[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    private[this] def loop[A](remaining: Long)(step: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (step.isDone) Step.done(step) else new StepCont[F, E, E, A](step) {
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (remaining <= 0L) {
            F.pure(Step.doneWithLeftovers(step, e :: Nil))
          } else {
            F.map(step.feedEl(e))(loop(remaining - 1L))
          }
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
          if (remaining > Int.MaxValue.toLong) {
            F.map(step.feed(chunk))(loop(remaining - chunk.size.toLong))
          } else {
            val (taken, left) = chunk.splitAt(remaining.toInt)

            if (taken.isEmpty) {
              F.pure(Step.doneWithLeftovers(step, left))
            } else if (left.isEmpty) {
              F.map(step.feed(taken))(loop(remaining - taken.size.toLong))
            } else {
              F.map(step.feed(taken))(Step.doneWithLeftovers(_, left))
            }
          }
      }

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = F.pure(loop(n)(step))
  }

  /**
   * An [[Enumeratee]] that tales values from a stream as long as they satisfy
   * the given predicate.
   */
  final def takeWhile[F[_], E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] =
    new PureLoop[F, E, E] {
      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (!p(e)) {
            F.pure(Step.doneWithLeftovers(step, e :: Nil))
          } else {
            F.map(step.feedEl(e))(doneOrLoop)
          }
        final def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = {
          val (taken, left) = chunk.span(p)

          if (taken.isEmpty) {
            F.pure(Step.doneWithLeftovers(step, left))
          } else if (left.isEmpty) {
            F.map(step.feed(taken))(doneOrLoop)
          } else {
            F.map(step.feed(taken))(Step.doneWithLeftovers(_, left))
          }
        }
      }
    }

  final def takeWhileM[F[_], E](p: E => F[Boolean])(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new PureLoop[F, E, E] {
      private[this] final def vectorSpanM[F[_], E](p: E => F[Boolean], v: Vector[E])(
        implicit F: Monad[F]
      ): F[(Vector[E], Vector[E])] = {
        def go(current: Vector[E], acc: Vector[E]): F[(Vector[E], Vector[E])] = current match {
          case vv @ e +: rest => F.ifM(p(e))(ifFalse = F.pure((acc, vv)), ifTrue = go(rest, acc :+ e))
          case vv => F.pure((acc, vv))
        }
        go(v, Vector.empty)
      }

      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = {
          F.ifM(p(e))(
            ifFalse = F.pure(Step.doneWithLeftovers(step, e :: Nil)),
            ifTrue  = F.map(step.feedEl(e))(doneOrLoop))
        }
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = {
          F.flatMap(vectorSpanM(p, chunk.toVector)) {
            case (taken, left) =>
              if (taken.isEmpty) {
                F.pure(Step.doneWithLeftovers(step, left))
              } else if (left.isEmpty) {
                F.map(step.feed(taken))(doneOrLoop)
              } else {
                F.map(step.feed(taken))(Step.doneWithLeftovers(_, left))
              }
          }
        }
      }
    }

  /**
   * An [[Enumeratee]] that drops a given number of the first values in a
   * stream.
   */
  final def drop[F[_], E](n: Long)(implicit F: Applicative[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    private[this] def loop[A](remaining: Long)(step: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (step.isDone) Step.done(step) else if (remaining <= 0L) new IdentityCont(step) else {
        new StepCont[F, E, E, A](step) {
          final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.pure(loop(remaining - 1)(step))
          final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
            if (remaining > Int.MaxValue.toLong) {
              F.pure(loop(remaining - chunk.size.toLong)(step))
            } else {
              val diff = remaining.toInt - chunk.size

              if (diff >= 0) {
                F.pure(loop(diff.toLong)(step))
              } else {
                F.map(step.feed(chunk.takeRight(-diff)))(new IdentityCont(_))
              }
            }
        }
      }

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = F.pure(loop(n)(step))
  }

  /**
   * An [[Enumeratee]] that drops values from a stream as long as they satisfy
   * the given predicate.
   */
  final def dropWhile[F[_], E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] =
    new PureLoop[F, E, E] {
      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (p(e)) F.pure(loop(step)) else F.map(step.feedEl(e))(new IdentityCont(_))
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = {
          val left = chunk.dropWhile(p)

          if (left.isEmpty) {
            F.pure(loop(step))
          } else {
            F.map(step.feed(left))(new IdentityCont(_))
          }
        }
      }
    }

  /**
   * An [[Enumeratee]] that drops values from a stream as long as they satisfy
   * the given monadic predicate.
   */
  final def dropWhileM[F[_], E](p: E => F[Boolean])(implicit F: Monad[F]): Enumeratee[F, E, E] =
    new PureLoop[F, E, E] {
      private[this] def vectorDropWhileM(p: E => F[Boolean], v: Vector[E]): F[Vector[E]] = {
        def go(current: Vector[E]): F[Vector[E]] = current match {
          case vv @ e +: rest => F.ifM(p(e))(ifFalse = F.pure(current), ifTrue = go(rest))
          case vv => F.pure(current)
        }
        go(v)
      }

      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          F.ifM(p(e))(ifTrue = F.pure(loop(step)), ifFalse = F.map(step.feedEl(e))(new IdentityCont(_)))
        final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
          F.flatMap(vectorDropWhileM(p, chunk.toVector)) { left =>
            if (left.isEmpty) {
              F.pure(loop(step))
            } else {
              F.map(step.feed(left))(new IdentityCont(_))
            }
          }
      }
    }

  /**
   * Transform values using a [[scala.PartialFunction]] and drop values that
   * aren't matched.
   */
  final def collect[F[_], O, I](pf: PartialFunction[O, I])(implicit F: Applicative[F]): Enumeratee[F, O, I] =
    new PureLoop[F, O, I] {
      protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new StepCont[F, O, I, A](step) {
        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = if (pf.isDefinedAt(e)) {
          F.map(step.feedEl(pf(e)))(doneOrLoop)
        } else {
          F.pure(loop(step))
        }
        final protected def feedNonEmpty(chunk: Seq[O]):F[Step[F, O, Step[F, I, A]]] = {
          val collected = chunk.collect(pf)

          if (collected.isEmpty) {
            F.pure(loop(step))
          } else {
            F.map(step.feed(collected))(doneOrLoop)
          }
        }
      }
    }

  /**
   * Drop values that do not satisfy the given predicate.
   *
   * @group Enumeratees
   */
  final def filter[F[_], E](p: E => Boolean)(implicit F: Applicative[F]): Enumeratee[F, E, E] = new PureLoop[F, E, E] {
    protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
        if (p(e)) F.map(step.feedEl(e))(doneOrLoop) else F.pure(loop(step))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = {
        val filtered = chunk.filter(p)

        if (filtered.isEmpty) {
          F.pure(loop(step))
        } else {
          F.map(step.feed(filtered))(doneOrLoop)
        }
      }
    }
  }

  /**
    * Drop values that do not satisfy a monadic predicate.
    */
  final def filterM[F[_], E](p: E => F[Boolean])(implicit F: Monad[F]): Enumeratee[F, E, E] = flatMap { e =>
    new Enumerator[F, E] {
      def apply[A](s: Step[F, E, A]): F[Step[F, E, A]] = F.ifM(p(e))(ifTrue = s.feedEl(e), ifFalse = F.pure(s))
    }
  }

  /**
   * Apply the given [[Iteratee]] repeatedly.
   */
  final def sequenceI[F[_], O, I](iteratee: Iteratee[F, O, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = F.pure(
        Step.tailRecM[F, O, Step[F, I, A], Step[F, I, A]](step) { s =>
          if (s.isDone) F.pure(Step.done(Right(s))) else {
            Step.isEnd[F, O].bind { isEnd =>
              if (isEnd) F.pure(Step.done(Right(s))) else {
                F.flatMap(iteratee.state)(
                  _.bind(i => F.map(s.feedEl(i))(nextStep => Step.done(Left(nextStep))))
                )
              }
            }
          }
        }
      )
    }

  /**
   * An [[Enumeratee]] that folds a stream and emits intermediate results.
   *
   * @group Collection
   */
  final def scan[F[_], O, I](init: I)(f: (I, O) => I)(implicit F: Applicative[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      protected def loop[A](current: I, step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        new StepCont[F, O, I, A](step) {
          final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = {
            val next = f(current, e)

            F.map(step.feedEl(next))(doneOrLoop(next))
          }
          final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] = {
            val results = chunk.tail.scanLeft(f(current, chunk.head))(f)

            F.map(step.feed(results))(doneOrLoop(results.last))
          }
        }

      protected final def doneOrLoop[A](current: I)(step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        if (step.isDone) Step.done(step) else loop(current, step)

      final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = F.map(step.feedEl(init))(doneOrLoop(init))
    }

  /**
   * An [[Enumeratee]] that folds a stream using an effectful function while
   * emitting intermediate results.
   *
   * @group Collection
   */
  final def scanM[F[_], O, I](init: I)(f: (I, O) => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      protected def loop[A](current: I, step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        new StepCont[F, O, I, A](step) {
          final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] =
            F.flatMap(f(current, e))(next => F.map(step.feedEl(next))(doneOrLoop(next)))

          final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] = {
            val pair = chunk.tail.foldLeft(
              F.flatMap(f(current, chunk.head))(next => F.map(step.feedEl(next))((next, _)))
            ) {
              case (pair, e) =>
                F.flatMap(pair) {
                  case (c, s) => F.flatMap(f(c, e))(next => F.map(s.feedEl(next))((next, _)))
                }
            }

            F.map(pair)(p => doneOrLoop(p._1)(p._2))
          }
        }

      protected final def doneOrLoop[A](current: I)(step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
        if (step.isDone) Step.done(step) else loop(current, step)

      final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = F.map(step.feedEl(init))(doneOrLoop(init))
    }

  /**
   * Run an iteratee and then use the provided function to combine the result
   * with the remaining elements.
   */
  final def remainderWithResult[F[_], O, R, I](iteratee: Iteratee[F, O, R])(f: (R, O) => I)(implicit
    F: Monad[F]
  ): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] class TransformingCont[A](r: R)(step: Step[F, I, A]) extends StepCont[F, O, I, A](step) {
        private[this] def advance(next: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
          if (next.isDone) Step.done(next) else new TransformingCont(r)(next)

        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(step.feedEl(f(r, e)))(advance)
        final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] =
          F.map(step.feed(chunk.map(f(r, _))))(advance)
      }

      final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
        F.flatMap(iteratee.state)(_.bind(r => F.pure(new TransformingCont(r)(step))))
    }

  /**
   * Run an iteratee and then use the provided effectful function to combine the
   * result with the remaining elements.
   */
  final def remainderWithResultM[F[_], O, R, I](iteratee: Iteratee[F, O, R])(f: (R, O) => F[I])(implicit
    F: Monad[F]
  ): Enumeratee[F, O, I] =
    new Enumeratee[F, O, I] {
      private[this] class TransformingCont[A](r: R)(step: Step[F, I, A]) extends StepCont[F, O, I, A](step) {
        private[this] def advance(next: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
          if (next.isDone) Step.done(next) else new TransformingCont(r)(next)

        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(F.flatMap(f(r, e))(step.feedEl))(advance)
        final protected def feedNonEmpty(chunk: Seq[O]): F[Step[F, O, Step[F, I, A]]] = F.map(
          chunk.tail.foldLeft(F.flatMap(f(r, chunk.head))(step.feedEl)) {
            case (next, e) => F.flatMap(next)(s => F.flatMap(f(r, e))(s.feedEl))
          }
        )(advance)
      }

      final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
        F.flatMap(iteratee.state)(_.bind(r => F.pure(new TransformingCont(r)(step))))
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
          final def run: F[A] = step.run
          final def feedEl(e: E): F[Step[F, E, A]] = last match {
            case Some(v) if E.eqv(e, v) => F.pure(stepWith(step, last))
            case _ => F.map(step.feedEl(e))(stepWith(_, Some(e)))
          }
          final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, A]] = {
            val (newEs, newLast) = chunk.foldLeft((Vector.empty[E], last)) {
              case ((acc, Some(lastValue)), e) if E.eqv(lastValue, e) => (acc, Some(lastValue))
              case ((acc, _), e) => (acc :+ e, Some(e))
            }

            if (newEs.isEmpty) {
              F.pure(stepWith(step, last))
            } else {
              F.map(step.feed(newEs))(stepWith(_, newLast))
            }
          }
        }

      final def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] =
        F.pure(stepWith(step, None).map(Step.done(_)))
    }

  /**
   * Zip with the number of elements that have been encountered.
   */
  final def zipWithIndex[F[_], E](implicit F: Applicative[F]): Enumeratee[F, E, (E, Long)] =
    new Enumeratee[F, E, (E, Long)] {
      private[this] final def doneOrLoop[A](i: Long)(step: Step[F, (E, Long), A]): Step[F, E, Step[F, (E, Long), A]] =
        if (step.isDone) Step.done(step) else stepWith(i, step)

      private[this] final def stepWith[A](i: Long, step: Step[F, (E, Long), A]): Step[F, E, Step[F, (E, Long), A]] =
        new StepCont[F, E, (E, Long), A](step) {
          final def feedEl(e: E): F[Step[F, E, Step[F, (E, Long), A]]] = F.map(step.feedEl((e, i)))(doneOrLoop(i + 1))
          final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, (E, Long), A]]] =
            F.map(step.feed(chunk.zipWithIndex.map(p => (p._1, p._2 + i))))(doneOrLoop(i + chunk.size))
        }

      final def apply[A](step: Step[F, (E, Long), A]): F[Step[F, E, Step[F, (E, Long), A]]] =
        F.pure(doneOrLoop(0L)(step))
    }

  /**
   * Split the stream into groups of a given length.
   */
  final def grouped[F[_]: Monad, E](n: Int): Enumeratee[F, E, Vector[E]] = sequenceI(Iteratee.take[F, E](n))

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
      private[this] final def loop[A](step: Step[F, (E1, E2), A]): F[Step[F, E1, Step[F, (E1, E2), A]]] =
        F.flatMap(Iteratee.head[F, E1].state)(
          _.bind {
            case Some(e) => F.flatMap(
              F.flatMap(Enumeratee.map[F, E2, (E1, E2)]((e, _)).apply(step))(e2.intoStep)
            )(loop)
            case None => F.pure(Step.done(step))
          }
        )

      final def apply[A](step: Step[F, (E1, E2), A]): F[Step[F, E1, Step[F, (E1, E2), A]]] = loop(step)
    }

  /**
   * Add a value `delim` between every two items in a stream.
   */
  final def intersperse[F[_], E](delim: E)(implicit F: Applicative[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    private[this] class FirstCont[A](step: Step[F, E, A]) extends StepCont[F, E, E, A](step) {
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.map(step.feedEl(e))(doneOrLoop(false))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
        F.map(step.feed(chunk.head +: beforeEvery(chunk.tail)))(doneOrLoop(false))
    }

    private[this] class RestCont[A](step: Step[F, E, A]) extends StepCont[F, E, E, A](step) {
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
        F.map(step.feed(List(delim, e)))(doneOrLoop(false))
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
        F.map(step.feed(delim +: chunk.head +: beforeEvery(chunk.tail)))(doneOrLoop(false))
    }

    private[this] final def beforeEvery(v: Seq[E]): Vector[E] = {
      val result = Vector.newBuilder[E]
      val it = v.iterator

      while (it.hasNext) {
        result += delim
        result += it.next()
      }

      result.result()
    }

    private[this] final def doneOrLoop[A](first: Boolean)(step: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (step.isDone) Step.done(step) else if (first) new FirstCont(step) else new RestCont(step)

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = F.pure(doneOrLoop(true)(step))
  }

  /**
   * Inject a value into a stream.
   */
  def injectValue[F[_], E](e: E)(implicit F: Monad[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = F.flatMap(step.feedEl(e))(identity[F, E].apply)
  }

  /**
   * Inject zero or more values into a stream.
   */
  def injectValues[F[_], E](es: Seq[E])(implicit F: Monad[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = F.flatMap(step.feed(es))(identity[F, E].apply)
  }

  /**
   * Observe the chunks in a stream.
   *
   * @note Typically you should never rely on the underlying chunking of a
   * stream, but in some cases it can be useful.
   */
  final def chunks[F[_], E](implicit F: Applicative[F]): Enumeratee[F, E, Vector[E]] =
    new PureLoop[F, E, Vector[E]] {
      protected def loop[A](step: Step[F, Vector[E], A]): Step[F, E, Step[F, Vector[E], A]] =
        new StepCont[F, E, Vector[E], A](step) {
          final def feedEl(e: E): F[Step[F, E, Step[F, Vector[E], A]]] =
            F.map(step.feedEl(Vector(e)))(doneOrLoop)
          final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, Vector[E], A]]] =
            F.map(step.feedEl(chunk.toVector))(doneOrLoop)
        }
    }

  private[this] final class Rechunk1[F[_], E](implicit F: Monad[F]) extends PureLoop[F, E, E] {
    protected def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new StepCont[F, E, E, A](step) {
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.map(step.feedEl(e))(doneOrLoop)
      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = F.map(
        chunk.tail.foldLeft(step.feedEl(chunk.head)) {
          case (next, e) => F.flatMap(next)(_.feedEl(e))
        }
      )(doneOrLoop)
    }
  }

  private[this] final class RechunkN[F[_], E](size: Int)(implicit F: Monad[F]) extends Enumeratee[F, E, E] {
    private[this] def freshBuilder(): Builder[E, Vector[E]] = {
      val builder = Vector.newBuilder[E]
      builder.sizeHint(size)
      builder
    }

    private[this] def loop[A](current: Int, acc: Builder[E, Vector[E]])(
      step: Step[F, E, A]
    ): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
      final def run: F[Step[F, E, A]] = step.feed(acc.result())

      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = if (current + 1 == size) {
        F.flatMap(step.feed((acc += e).result()))(doneOrLoop(0, freshBuilder()))
      } else F.pure(loop(current + 1, acc += e)(step))

      final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] = {
        val c = chunk.lengthCompare(size - current)

        if (c < 0) F.pure(loop(current + chunk.size, acc ++= chunk)(step)) else if (c == 0) {
          F.flatMap(step.feed((acc ++= chunk).result()))(doneOrLoop(0, freshBuilder()))
        } else {
          val newChunks = (acc ++= chunk).result().grouped(size)

          val (nextStep, lastChunk) = newChunks.foldLeft((F.pure(step), Vector.empty[E])) {
            case ((ns, lc), es) =>
              if (es.size == size) (F.flatMap(ns)(_.feed(es)), Vector.empty) else (ns, es)
          }

          F.flatMap(nextStep)(doneOrLoop(lastChunk.size, freshBuilder() ++= lastChunk))
        }
      }
    }

    private[this] final def doneOrLoop[A](current: Int, acc: Builder[E, Vector[E]])(
      step: Step[F, E, A]
    ): F[Step[F, E, Step[F, E, A]]] = if (step.isDone) {
      if (current == 0) F.pure(Step.done(step)) else {
        F.map(step.feed(acc.result()))(Step.done[F, E, Step[F, E, A]](_))
      }
    } else F.pure(loop(current, acc)(step))

    final def apply[A](step: Step[F, E, A]): F[Step[F, E, Step[F, E, A]]] = doneOrLoop(0, freshBuilder())(step)
  }

  /**
   * Rechunk elements in the stream into chunks of the provided size.
   */
  final def rechunk[F[_], E](size: Int)(implicit F: Monad[F]): Enumeratee[F, E, E] =
    if (size <= 1) new Rechunk1[F, E] else new RechunkN[F, E](size)

  private[this] abstract class StepCont[F[_], O, I, A](step: Step[F, I, A])(implicit
    F: Applicative[F]
  ) extends Step.Cont[F, O, Step[F, I, A]] {
    final def run: F[Step[F, I, A]] = F.pure(step)
  }

  private[this] final class IdentityCont[F[_], E, A](step: Step[F, E, A])(implicit
    F: Applicative[F]
  ) extends StepCont[F, E, E, A](step) {
    private[this] def advance(next: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (next.isDone) Step.done(next) else new IdentityCont(next)

    final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.map(step.feedEl(e))(advance)
    final protected def feedNonEmpty(chunk: Seq[E]): F[Step[F, E, Step[F, E, A]]] =
      F.map(step.feed(chunk))(advance)
  }

  private[this] abstract class PureLoop[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]]

    protected final def doneOrLoop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
      if (step.isDone) Step.done(step) else loop(step)

    final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = F.pure(doneOrLoop(step))
  }
}
