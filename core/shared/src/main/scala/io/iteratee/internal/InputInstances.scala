package io.iteratee.internal

import algebra.{ Eq, Semigroup }
import cats.{ Applicative, Eval, Foldable, Monad, Show, Traverse }
import cats.std.VectorInstances

private[iteratee] trait InputInstances extends VectorInstances {
  implicit final val inputInstances: Traverse[Input] with Monad[Input] = new Traverse[Input] with Monad[Input] {
    final def pure[A](a: A): Input[A] = Input.el(a)
    final def traverse[G[_], A, B](fa: Input[A])(f: A => G[B])(implicit G: Applicative[G]): G[Input[B]] = fa.foldWith(
      new Input.Folder[A, G[Input[B]]] {
        final def onEmpty: G[Input[B]] = G.pure(Input.empty[B])
        final def onEl(e: A): G[Input[B]] = G.map(f(e))(Input.el)
        final def onChunk(es: Vector[A]): G[Input[B]] = G.map(Traverse[Vector].traverse[G, A, B](es)(f))(Input.chunk)
        final def onEnd: G[Input[B]] = G.pure(Input.end[B])
      }
    )

    final def foldLeft[A, B](fa: Input[A], b: B)(f: (B, A) => B): B = fa.foldWith(
      new Input.Folder[A, B] {
        final def onEmpty: B = b
        final def onEl(e: A): B = f(b, e)
        final def onChunk(es: Vector[A]): B = Foldable[Vector].foldLeft(es, b)(f)
        final def onEnd: B = b
      }
    )

    final def foldRight[A, B](fa: Input[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldWith(
      new Input.Folder[A, Eval[B]] {
        final def onEmpty: Eval[B] = b
        final def onEl(e: A): Eval[B] = f(e, b)
        final def onChunk(es: Vector[A]): Eval[B] = Foldable[Vector].foldRight(es, b)(f)
        final def onEnd: Eval[B] = b
      }
    )

    override final def filter_[A](fa: Input[A])(p: A => Boolean): List[A] = fa.filter(p).toVector.toList
    override final def exists[A](fa: Input[A])(p: A => Boolean): Boolean = fa.exists(p)
    override final def forall[A](fa: Input[A])(p: A => Boolean): Boolean = fa.forall(p)
    override final def toList[A](fa: Input[A]): List[A] = fa.toVector.toList
    override final def isEmpty[A](fa: Input[A]): Boolean = fa.isEmpty || fa.isEnd
    override final def map[A, B](fa: Input[A])(f: A => B): Input[B] = fa.map(f)
    final def flatMap[A, B](fa: Input[A])(f: A => Input[B]): Input[B] = fa.flatMap(f)
  }

  implicit final def inputSemigroup[A](implicit A: Semigroup[A]): Semigroup[Input[A]] = new Semigroup[Input[A]] {
    final def combine(a1: Input[A], a2: Input[A]): Input[A] = a1.normalize.foldWith(
      new Input.Folder[A, Input[A]] {
        final def onEmpty: Input[A] = a2.normalize.foldWith(
          new Input.Folder[A, Input[A]] {
            final def onEmpty: Input[A] = a2
            final def onEl(e: A): Input[A] = a2
            final def onChunk(es: Vector[A]): Input[A] = Input.el(es.reduceLeft(A.combine))
            final def onEnd: Input[A] = Input.end
          }
        )
        final def onEl(e: A): Input[A] = a2.foldWith(
          new Input.Folder[A, Input[A]] {
            final def onEmpty: Input[A] = Input.el(e)
            final def onEl(f: A): Input[A] = Input.el(A.combine(e, f))
            final def onChunk(fs: Vector[A]): Input[A] = Input.el(fs.foldLeft(e)(A.combine))
            final def onEnd: Input[A] = Input.end
          }
        )
        final def onChunk(es: Vector[A]): Input[A] = a2.foldWith(
          new Input.Folder[A, Input[A]] {
            final def onEmpty: Input[A] = Input.el(es.reduceLeft(A.combine))
            final def onEl(e: A): Input[A] = Input.el((es :+ e).reduceLeft(A.combine))
            final def onChunk(fs: Vector[A]): Input[A] = Input.el((es ++ fs).reduceLeft(A.combine))
            final def onEnd: Input[A] = Input.end
          }
        )
        final def onEnd: Input[A] = Input.end
      }
    )
  }

  implicit final def inputEq[A](implicit A: Eq[A]): Eq[Input[A]] = new Eq[Input[A]] {
    private[this] final lazy val eqVectorA: Eq[Vector[A]] = Eq[Vector[A]]

    final def eqv(a1: Input[A], a2: Input[A]): Boolean = a1.normalize.foldWith(
      new Input.Folder[A, Boolean] {
        final def onEmpty: Boolean = a2.isEmpty
        final def onEl(e: A): Boolean = a2.exists(f => A.eqv(e, f))
        final def onChunk(es: Vector[A]): Boolean = a2.foldWith(
          new Input.Folder[A, Boolean] {
            final def onEmpty: Boolean = false
            final def onEl(e: A): Boolean = false
            final def onChunk(fs: Vector[A]): Boolean = eqVectorA.eqv(es, fs)
            final def onEnd: Boolean = false
          }
        )
        final def onEnd: Boolean = a2.isEnd
      }
    )
  }

  implicit final def inputShow[A](implicit A: Show[A]): Show[Input[A]] = new Show[Input[A]] {
    override final def show(f: Input[A]): String = f.foldWith(
      new Input.Folder[A, String] {
        final def onEmpty: String = "empty"
        final def onEl(e: A): String = s"el(${ A.show(e) })"
        final def onChunk(es: Vector[A]): String =
          s"""chunk(${ es.map(A.show).mkString(", ") })"""
        final def onEnd: String = "end"
      }
    )
  }
}
