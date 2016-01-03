package io.iteratee.internal

import algebra.Eq
import cats.Show
import cats.std.VectorInstances

private[iteratee] trait InputInstances extends VectorInstances {
  implicit final def inputEq[A](implicit A: Eq[A]): Eq[Input[A]] = new Eq[Input[A]] {
    private[this] final lazy val eqVectorA: Eq[Vector[A]] = Eq[Vector[A]]

    final def eqv(a1: Input[A], a2: Input[A]): Boolean = a1.foldWith(
      new Input.Folder[A, Boolean] {
        final def onEl(e: A): Boolean = a2.foldWith(
          new Input.Folder[A, Boolean] {
            final def onEl(e2: A): Boolean = A.eqv(e, e2)
            final def onChunk(f1: A, f2: A, fs: Vector[A]): Boolean = false
            //final def onEndX: Boolean = false
          }
        )
        final def onChunk(e1: A, e2: A, es: Vector[A]): Boolean = a2.foldWith(
          new Input.Folder[A, Boolean] {
            final def onEl(e: A): Boolean = false
            final def onChunk(f1: A, f2: A, fs: Vector[A]): Boolean =
              A.eqv(e1, f1) && A.eqv(e2, f2) && eqVectorA.eqv(es, fs)
            //final def onEndX: Boolean = false
          }
        )
        //final def onEndX: Boolean = a2.isEnd
      }
    )
  }

  implicit final def inputShow[A](implicit A: Show[A]): Show[Input[A]] = new Show[Input[A]] {
    override final def show(in: Input[A]): String = in.foldWith(
      new Input.Folder[A, String] {
        final def onEl(e: A): String = s"el(${ A.show(e) })"
        final def onChunk(h1: A, h2: A, es: Vector[A]): String =
          s"""chunk(${ in.toVector.map(A.show).mkString(", ") })"""
        //final def onEndX: String = "end"
      }
    )
  }
}
