package io.iteratee

import algebra.Monoid
import cats.{ Applicative, Monad }
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

private[iteratee] trait CollectionSteps {
  final def fold[F[_]: Applicative, E, A](init: A)(f: (A, E) => A): Step[F, E, A] = {
    def step(acc: A)(in: Input[E]): Step[F, E, A] = in.foldWith(
      new InputFolder[E, Step[F, E, A]] {
        def onEmpty: Step[F, E, A] = Step.pureCont(step(acc))
        def onEl(e: E): Step[F, E, A] = Step.pureCont(step(f(acc, e)))
        def onChunk(es: Vector[E]): Step[F, E, A] = Step.pureCont(step(es.foldLeft(acc)(f)))
        def onEnd: Step[F, E, A] = Step.done(acc, Input.end)
      }
    )

    Step.pureCont(step(init))
  }

  final def foldM[F[_], E, A](init: A)(f: (A, E) => F[A])(implicit F: Monad[F]): Step[F, E, A] = {
    def step(acc: A)(in: Input[E]): F[Step[F, E, A]] = in.foldWith(
      new InputFolder[E, F[Step[F, E, A]]] {
        def onEmpty: F[Step[F, E, A]] = F.pure(Step.cont(step(acc)))
        def onEl(e: E): F[Step[F, E, A]] = F.map(f(acc, e))(a => Step.cont(step(a)))
        def onChunk(es: Vector[E]): F[Step[F, E, A]] =
          F.map(es.foldLeft(F.pure(acc))((fa, e) => F.flatMap(fa)(a => f(a, e))))(a =>
            Step.cont(step(a))
          )
        def onEnd: F[Step[F, E, A]] = F.pure(Step.done(acc, Input.end))
      }
    )

    Step.cont(step(init))
  }

  final def consume[F[_], A](implicit F: Applicative[F]): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A])(in: Input[A]): Step[F, A, Vector[A]] = in.foldWith(
      new InputFolder[A, Step[F, A, Vector[A]]] {
        def onEmpty: Step[F, A, Vector[A]] = Step.pureCont(a => loop(acc)(a))
        def onEl(e: A): Step[F, A, Vector[A]] = Step.pureCont(a => loop(acc :+ e)(a))
        def onChunk(es: Vector[A]): Step[F, A, Vector[A]] =
          Step.pureCont(a => loop(acc ++ es)(a))
        def onEnd: Step[F, A, Vector[A]] = Step.done(acc, in)
      }
    )

    Step.pureCont(a => loop(Vector.empty)(a))
  }

  final def consumeIn[F[_], A, C[_]](implicit
    F: Monad[F],
    cbf: CanBuildFrom[Nothing, A, C[A]]
  ): Step[F, A, C[A]] = {
    def loop(acc: Builder[A, C[A]])(in: Input[A]): Step[F, A, C[A]] = in.foldWith(
      new InputFolder[A, Step[F, A, C[A]]] {
        def onEmpty: Step[F, A, C[A]] = Step.pureCont(loop(acc))
        def onEl(e: A): Step[F, A, C[A]] = Step.pureCont(loop(acc += e))
        def onChunk(es: Vector[A]): Step[F, A, C[A]] = Step.pureCont(loop(acc ++= es))
        def onEnd: Step[F, A, C[A]] = Step.done(acc.result(), in)
      }
    )

    Step.pureCont(loop(cbf()))
  }

  final def head[F[_]: Applicative, E]: Step[F, E, Option[E]] = {
    def step(in: Input[E]): Step[F, E, Option[E]] = in.foldWith(
      new InputFolder[E, Step[F, E, Option[E]]] {
        def onEmpty: Step[F, E, Option[E]] = Step.pureCont(step)
        def onEl(e: E): Step[F, E, Option[E]] = Step.done(Some(e), Input.empty)
        def onChunk(es: Vector[E]): Step[F, E, Option[E]] =
          if (es.isEmpty) onEmpty else Step.done(Some(es.head), Input.chunk(es.tail))
        def onEnd: Step[F, E, Option[E]] = Step.done(None, in)
      }
    )

    Step.pureCont(step)
  }

  final def peek[F[_]: Applicative, E]: Step[F, E, Option[E]] = {
    def step(in: Input[E]): Step[F, E, Option[E]] = in.foldWith(
      new InputFolder[E, Step[F, E, Option[E]]] {
        def onEmpty: Step[F, E, Option[E]] = Step.pureCont(step)
        def onEl(e: E): Step[F, E, Option[E]] = Step.done(Some(e), in)
        def onChunk(es: Vector[E]): Step[F, E, Option[E]] =
          if (es.isEmpty) onEmpty else Step.done(Some(es.head), in)
        def onEnd: Step[F, E, Option[E]] = Step.done(None, in)
      }
    )

    Step.pureCont(step)
  }

  final def take[F[_]: Applicative, A](n: Int): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A], n: Int)(in: Input[A]): Step[F, A, Vector[A]] = in.foldWith(
      new InputFolder[A, Step[F, A, Vector[A]]] {
        def onEmpty: Step[F, A, Vector[A]] = Step.pureCont(loop(acc, n))
        def onEl(e: A): Step[F, A, Vector[A]] =
          if (n == 1) Step.done(acc :+ e, Input.empty) else Step.pureCont(loop(acc :+ e, n - 1))
        def onChunk(es: Vector[A]): Step[F, A, Vector[A]] = {
          val diff = n - es.size

          if (diff > 0) Step.pureCont(loop(acc ++ es, diff)) else {
            if (diff == 0) Step.done(acc ++ es, Input.empty) else {
              val (taken, left) = es.splitAt(n)

              Step.done(acc ++ taken, Input.chunk(left))
            }
          }
        }
        def onEnd: Step[F, A, Vector[A]] = Step.done(acc, in)
      }
    )

    if (n <= 0) {
      Step.done[F, A, Vector[A]](Vector.empty, Input.empty)
    } else {
      Step.pureCont(loop(Vector.empty, n))
    }
  }

  final def takeWhile[F[_]: Applicative, A](p: A => Boolean): Step[F, A, Vector[A]] = {
    def loop(acc: Vector[A])(in: Input[A]): Step[F, A, Vector[A]] = in.foldWith(
      new InputFolder[A, Step[F, A, Vector[A]]] {
        def onEmpty: Step[F, A, Vector[A]] = Step.pureCont(loop(acc))
        def onEl(e: A): Step[F, A, Vector[A]] =
          if (p(e)) Step.pureCont(loop(acc :+ e)) else Step.done(acc, in)

        def onChunk(es: Vector[A]): Step[F, A, Vector[A]] = {
          val (before, after) = es.span(p)

          if (after.isEmpty) {
            Step.pureCont(loop(acc ++ before))
          } else {
            Step.done(acc ++ before, Input.chunk(after))
          }
        }
        def onEnd: Step[F, A, Vector[A]] = Step.done(acc, in)
      }
    )

    Step.pureCont(loop(Vector.empty))
  }

  final def drop[F[_]: Applicative, E](n: Int): Step[F, E, Unit] = {
    def step(in: Input[E]): Step[F, E, Unit] = in.foldWith(
      new InputFolder[E, Step[F, E, Unit]] {
        def onEmpty: Step[F, E, Unit] = Step.pureCont(step)
        def onEl(e: E): Step[F, E, Unit] = drop(n - 1)
        def onChunk(es: Vector[E]): Step[F, E, Unit] = {
          val len = es.size

          if (len <= n) drop(n - len) else Step.done((), Input.chunk(es.drop(n)))
        }
        def onEnd: Step[F, E, Unit] = Step.done((), in)
      }
    )

    if (n <= 0) Step.done((), Input.empty) else Step.pureCont(step)
  }

  final def dropWhile[F[_], E](p: E => Boolean)(implicit F: Applicative[F]): Step[F, E, Unit] =
    new StepUnitInputFolder[F, E] {
      final def onEl(e: E): Step[F, E, Unit] =
        if (p(e)) dropWhile(p) else Step.done((), Input.el(e))
      final def onChunk(es: Vector[E]): Step[F, E, Unit] = {
        val after = es.dropWhile(p)

        if (after.isEmpty) dropWhile(p) else Step.done((), Input.chunk(after))
      }
    }.onEmpty
}

private[iteratee] abstract class StepInputFolder[F[_], E, A](implicit F: Applicative[F])
  extends InputFolder[E, Step[F, E, A]] {
  final def next(in: Input[E]): Step[F, E, A] = in.foldWith(this)

  final def onEmpty: Step[F, E, A] = Step.pureCont(next)
}

private[iteratee] abstract class StepUnitInputFolder[F[_], E](implicit F: Applicative[F])
  extends StepInputFolder[F, E, Unit] {
  final def onEnd: Step[F, E, Unit] = StepUnitInputFolder.done.asInstanceOf[Step[F, E, Unit]]
}

private[iteratee] object StepUnitInputFolder {
  final val done: Step[Nothing, Nothing, Unit] = Step.done[Nothing, Nothing, Unit]((), Input.end)
}
