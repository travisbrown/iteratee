package io.iteratee

import cats.{ Applicative, Eq, FlatMap, Monad }
import cats.instances.list.catsStdInstancesForList
import io.iteratee.internal.Step
import scala.Predef.$conforms

abstract class Enumeratee[F[_], O, I] extends Serializable { self =>
  def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]]

  final def wrap(enum: Enumerator[F, O])(implicit F: FlatMap[F]): Enumerator[F, I] = new Enumerator[F, I] {
    final def apply[A](s: Step[F, I, A]): F[Step[F, I, A]] = F.flatMap(self(s))(enum.intoStep)
  }

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
  private[this] class IdentityCont[F[_], E, A](step: Step[F, E, A])(implicit
    F: Applicative[F]
  ) extends Step.Cont[F, E, Step[F, E, A]] {
    private[this] def advance(next: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (next.isDone) Step.done(next) else new IdentityCont(next)

    final def run: F[Step[F, E, A]] = F.pure(step)
    final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.map(step.feedEl(e))(advance)
    final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] =
      F.map(step.feed(chunk))(advance)
  }

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
    protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new Step.Cont[F, O, Step[F, I, A]] {
      final def run: F[Step[F, I, A]] = F.pure(step)
      final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(step.feedEl(f(e)))(doneOrLoop)
      final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, O]): F[Step[F, O, Step[F, I, A]]] =
        F.map(step.feed(isl.conversion(chunk).toSeq.map(f)))(doneOrLoop)
    }
  }

  /**
   * Map a function returning a value in a context over a stream.
   */
  final def flatMapM[F[_], O, I](f: O => F[I])(implicit F: Monad[F]): Enumeratee[F, O, I] = new PureLoop[F, O, I] {
    protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new Step.Cont[F, O, Step[F, I, A]] {
      final def run: F[Step[F, I, A]] = F.pure(step)
      final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]]= F.map(F.flatMap(f(e))(step.feedEl))(doneOrLoop)
      final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, O]): F[Step[F, O, Step[F, I, A]]] = F.map(
        F.flatMap(catsStdInstancesForList.traverse(isl.conversion(chunk).toList)(f))(step.feed(_))
      )(doneOrLoop)
    }
  }

  /**
   * Map a function returning an [[Enumerator]] over a stream and flatten the
   * results.
   */
  final def flatMap[F[_], O, I](f: O => Enumerator[F, I])(implicit F: Monad[F]): Enumeratee[F, O, I] =
    new PureLoop[F, O, I] {
      protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new Step.Cont[F, O, Step[F, I, A]] {
        final def run: F[Step[F, I, A]] = F.pure(step)
        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = F.map(f(e)(step))(doneOrLoop)
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, O]): F[Step[F, O, Step[F, I, A]]] = {
          val s = isl.conversion(chunk)

          F.map(isl.conversion(s.tail).foldLeft(f(s.head))((acc, e) => acc.append(f(e))).apply(step))(doneOrLoop)
        }
      }
    }

  /**
   * An [[Enumeratee]] that takes a given number of the first values in a
   * stream.
   */
  final def take[F[_], E](n: Long)(implicit F: Applicative[F]): Enumeratee[F, E, E] = new Enumeratee[F, E, E] {
    private[this] def loop[A](remaining: Long)(step: Step[F, E, A]): Step[F, E, Step[F, E, A]] =
      if (step.isDone) Step.done(step) else new Step.Cont[F, E, Step[F, E, A]] {
        final def run: F[Step[F, E, A]] = F.pure(step)
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (remaining <= 0L) {
            F.pure(Step.Done(step, List(e)))
          } else {
            F.map(step.feedEl(e))(loop(remaining - 1L))
          }
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] =
          if (remaining > Int.MaxValue.toLong) {
            F.map(step.feed(chunk))(loop(remaining - isl.conversion(chunk).size.toLong))
          } else {
            val (taken, left) = isl.conversion(chunk).splitAt(remaining.toInt)
            val ts = isl.conversion(taken)
            val ls = isl.conversion(left)

            if (ts.isEmpty) {
              F.pure(Step.Done(step, ls.toSeq))
            } else if (ls.isEmpty) {
              F.map(step.feed(taken))(loop(remaining - ts.size.toLong))
            } else {
              F.map(step.feed(taken))(Step.Done(_, ls.toSeq))
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
      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
        final def run: F[Step[F, E, A]] = F.pure(step)
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (!p(e)) {
            F.pure(Step.Done(step, List(e)))
          } else {
            F.map(step.feedEl(e))(doneOrLoop)
          }
        final def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
          val (taken, left) = isl.conversion(chunk).span(p)
          val ts = isl.conversion(taken)
          val ls = isl.conversion(left)

          if (ts.isEmpty) {
            F.pure(Step.Done(step, ls.toSeq))
          } else if (ls.isEmpty) {
            F.map(step.feed(taken))(doneOrLoop)
          } else {
            F.map(step.feed(taken))(Step.Done(_, ls.toSeq))
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

      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
        final def run: F[Step[F, E, A]] = F.pure(step)
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = {
          F.ifM(p(e))(
            ifFalse = F.pure(Step.Done(step, List(e))),
            ifTrue  = F.map(step.feedEl(e))(doneOrLoop))
        }
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
          F.flatMap(vectorSpanM(p, isl.conversion(chunk).toVector)) {
            case (taken, left) =>
              if (taken.isEmpty) {
                F.pure(Step.Done(step, left))
              } else if (left.isEmpty) {
                F.map(step.feed(taken))(doneOrLoop)
              } else {
                F.map(step.feed(taken))(Step.Done(_, left))
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
        new Step.Cont[F, E, Step[F, E, A]] {
          final def run: F[Step[F, E, A]] = F.pure(step)
          final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.pure(loop(remaining - 1)(step))
          final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] =
            if (remaining > Int.MaxValue.toLong) {
              F.pure(loop(remaining - isl.conversion(chunk).size.toLong)(step))
            } else {
              val s = isl.conversion(chunk)
              val diff = remaining.toInt - s.size

              if (diff >= 0) {
                F.pure(loop(diff.toLong)(step))
              } else {
                F.map(step.feed(s.takeRight(-diff)))(new IdentityCont(_))
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
      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
        final def run: F[Step[F, E, A]] = F.pure(step)
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          if (p(e)) F.pure(loop(step)) else F.map(step.feedEl(e))(new IdentityCont(_))
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
          val left = isl.conversion(chunk).dropWhile(p)

          if (isl.conversion(left).isEmpty) {
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

      protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
        final def run: F[Step[F, E, A]] = F.pure(step)
        final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
          F.ifM(p(e))(ifTrue = F.pure(loop(step)), ifFalse = F.map(step.feedEl(e))(new IdentityCont(_)))
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] =
          F.flatMap(vectorDropWhileM(p, isl.conversion(chunk).toVector)) { left =>
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
      protected final def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] = new Step.Cont[F, O, Step[F, I, A]] {
        final def run: F[Step[F, I, A]] = F.pure(step)
        final def feedEl(e: O): F[Step[F, O, Step[F, I, A]]] = if (pf.isDefinedAt(e)) {
          F.map(step.feedEl(pf(e)))(doneOrLoop)
        } else {
          F.pure(loop(step))
        }
        final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, O]): F[Step[F, O, Step[F, I, A]]] = {
          val collected = isl.conversion(chunk).toSeq.collect(pf)

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
    protected final def loop[A](step: Step[F, E, A]): Step[F, E, Step[F, E, A]] = new Step.Cont[F, E, Step[F, E, A]] {
      final def run: F[Step[F, E, A]] = F.pure(step)
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
        if (p(e)) F.map(step.feedEl(e))(doneOrLoop) else F.pure(loop(step))
      final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
        val filtered = isl.conversion(chunk).filter(p)

        if (isl.conversion(filtered).isEmpty) {
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
    new EffectfulLoop[F, O, I] {
      protected final def loop[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
        Step.isEnd[F, O].bind { isEnd =>
          if (isEnd) F.pure(Step.done(step)) else F.flatMap(iteratee.state)(
            _.bind(a => F.flatMap(step.feedEl(a))(doneOrLoop))
          )
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
          final def run: F[A] = step.run
          final def feedEl(e: E): F[Step[F, E, A]] = last match {
            case Some(v) if E.eqv(e, v) => F.pure(stepWith(step, last))
            case _ => F.map(step.feedEl(e))(stepWith(_, Some(e)))
          }
          final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, A]] = {
            val (newEs, newLast) = isl.conversion(chunk).foldLeft((Vector.empty[E], last)) {
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
        new Step.Cont[F, E, Step[F, (E, Long), A]] {
          final def run: F[Step[F, (E, Long), A]] = F.pure(step)
          final def feedEl(e: E): F[Step[F, E, Step[F, (E, Long), A]]] = F.map(step.feedEl((e, i)))(doneOrLoop(i + 1))
          final protected def feedNonEmpty[C](chunk: C)(implicit
            isl: Step.ISL[C, E]
          ): F[Step[F, E, Step[F, (E, Long), A]]] = {
            val s = isl.conversion(chunk)
            F.map(step.feed(s.toSeq.zipWithIndex.map(p => (p._1, p._2 + i))))(doneOrLoop(i + s.size))
          }
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
    private[this] class FirstCont[A](step: Step[F, E, A]) extends Step.Cont[F, E, Step[F, E, A]] {
      final def run: F[Step[F, E, A]] = F.pure(step)
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] = F.map(step.feedEl(e))(doneOrLoop(false))
      final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
        val s = isl.conversion(chunk)

        F.map(step.feed(s.head +: beforeEvery(s.toSeq.tail)))(doneOrLoop(false))
      }
    }

    private[this] class RestCont[A](step: Step[F, E, A]) extends Step.Cont[F, E, Step[F, E, A]] {
      final def run: F[Step[F, E, A]] = F.pure(step)
      final def feedEl(e: E): F[Step[F, E, Step[F, E, A]]] =
        F.map(step.feed(List(delim, e)))(doneOrLoop(false))
      final protected def feedNonEmpty[C](chunk: C)(implicit isl: Step.ISL[C, E]): F[Step[F, E, Step[F, E, A]]] = {
        val s = isl.conversion(chunk)

        F.map(step.feed(delim +: s.head +: beforeEvery(s.toSeq.tail)))(doneOrLoop(false))
      }
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

  abstract class PureLoop[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]]

    protected final def doneOrLoop[A](step: Step[F, I, A]): Step[F, O, Step[F, I, A]] =
      if (step.isDone) Step.done(step) else loop(step)

    final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = F.pure(doneOrLoop(step))
  }

  abstract class EffectfulLoop[F[_], O, I](implicit F: Applicative[F]) extends Enumeratee[F, O, I] {
    protected def loop[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]]

    protected final def doneOrLoop[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] =
      if (step.isDone) F.pure(Step.done(step)) else loop(step)

    final def apply[A](step: Step[F, I, A]): F[Step[F, O, Step[F, I, A]]] = doneOrLoop(step)
  }
}
