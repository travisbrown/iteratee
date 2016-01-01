package io.iteratee.internal

import algebra.Eq
import cats.Show
import cats.std.VectorInstances

private[iteratee] trait InputInstances extends VectorInstances {
  implicit final def inputEq[A](implicit A: Eq[A]): Eq[Input[A]] = new Eq[Input[A]] {
    private[this] final lazy val eqVectorA: Eq[Vector[A]] = Eq[Vector[A]]

    final def eqv(a1: Input[A], a2: Input[A]): Boolean = a1.normalize.foldWith(
      new Input.Folder[A, Boolean] {
        final def onEl(e: A): Boolean = a2.foldWith(
          new Input.Folder[A, Boolean] {
            final def onEl(e2: A): Boolean = A.eqv(e, e2)
            final def onChunk(fs: Vector[A]): Boolean = false
            final def onEnd: Boolean = false
          }
        )
        final def onChunk(es: Vector[A]): Boolean = a2.foldWith(
          new Input.Folder[A, Boolean] {
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
