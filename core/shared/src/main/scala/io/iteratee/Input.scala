package io.iteratee

import algebra.{ Eq, Semigroup }
import cats.{ Applicative, Eval, Foldable, Monad, SemigroupK, Show, Traverse }
import cats.std.VectorInstances

/**
 * Represents four functions that can be used to reduce an [[Input]] to a value.
 *
 * Combining two "functions" into a single class allows us to save allocations. `onEmpty` and `onEl`
 * may be overriden to avoid unnecessary allocations, but should be consistent with `onChunk`.
 */
abstract class InputFolder[@specialized E, A] extends Serializable {
  def onChunk(es: Vector[E]): A
  def onEnd: A
  def onEmpty: A
  def onEl(e: E): A
}

/**
 * Input to an [[Iteratee]].
 */
sealed abstract class Input[@specialized E] extends Serializable { self =>
  /**
   * Reduce this [[Input]] to a value using the given four functions.
   */
  def foldWith[A](folder: InputFolder[E, A]): A

  def isEmpty: Boolean
  def isEnd: Boolean

  def map[X](f: E => X): Input[X]
  def flatMap[X](f: E => Input[X]): Input[X]
  def filter(f: E => Boolean): Input[E]
  def foreach(f: E => Unit): Unit
  def forall(p: E => Boolean): Boolean
  def exists(p: E => Boolean): Boolean

  /**
   * Normalize the [[Input]] so that representations do not overlap.
   *
   * If this [[Input]] is a chunk with no values, an empty input will be returned, and if it's a
   * chunk with a single value, and element input will be returned.
   */
  private[iteratee] def normalize: Input[E]

  /**
   * Convert this [[Input]] value into a list of elements.
   */
  private[iteratee] def toVector: Vector[E]

  private[iteratee] final def shorter(that: Input[E]): Input[E] =
    if (isEnd || that.isEnd) Input.end
      else if (isEmpty || that.isEmpty) Input.empty
      else if (toVector.lengthCompare(that.toVector.size) < 0) this else that
}

object Input extends InputInstances {
  /**
   * An empty input value.
   */
  final def empty[E]: Input[E] = emptyValue.asInstanceOf[Input[E]]

  /**
   * An input value representing the end of a stream.
   */
  final def end[E]: Input[E] = endValue.asInstanceOf[Input[E]]

  /**
   * An input value containing a single element.
   */
  final def el[E](e: E): Input[E] = new Input[E] { self =>
    final def foldWith[A](folder: InputFolder[E, A]): A = folder.onEl(e)
    final def isEmpty: Boolean = false
    final def isEnd: Boolean = false
    final def map[X](f: E => X): Input[X] = Input.el(f(e))
    final def flatMap[X](f: E => Input[X]): Input[X] = f(e)
    final def filter(f: E => Boolean): Input[E] = if (f(e)) self else empty
    final def foreach(f: E => Unit): Unit = f(e)
    final def forall(p: E => Boolean): Boolean = p(e)
    final def exists(p: E => Boolean): Boolean = p(e)
    private[iteratee] final def normalize: Input[E] = self
    private[iteratee] final def toVector: Vector[E] = Vector(e)

  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](es: Vector[E]): Input[E] = new Input[E] { self =>
    final def foldWith[A](folder: InputFolder[E, A]): A = folder.onChunk(es)
    final def isEmpty: Boolean = es.isEmpty
    final def isEnd: Boolean = false
    final def map[X](f: E => X): Input[X] = chunk(es.map(f(_)))
    final def flatMap[X](f: E => Input[X]): Input[X] = es.foldLeft(empty[X]) {
      case (acc, _) if acc.isEnd => end
      case (acc, e) =>
        val ei = f(e)
        if (ei.isEnd) end else chunk(acc.toVector ++ ei.toVector)
    }
    final def filter(f: E => Boolean): Input[E] = Input.chunk(es.filter(f))
    final def foreach(f: E => Unit): Unit = es.foreach(f(_))
    final def forall(p: E => Boolean): Boolean = es.forall(p(_))
    final def exists(p: E => Boolean): Boolean = es.exists(p(_))

    private[iteratee] final def normalize: Input[E] = {
      val c = es.lengthCompare(1)
      if (c < 0) empty else if (c == 0) el(es.head) else self
    }

    private[iteratee] final def toVector: Vector[E] = es
  }

  private[this] final val emptyValue: Input[Nothing] = new Input[Nothing] {
    def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEmpty
    final val isEmpty: Boolean = true
    final val isEnd: Boolean = false
    final def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    final def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    final def filter(f: Nothing => Boolean): Input[Nothing] = this
    final def foreach(f: Nothing => Unit): Unit = ()
    final def forall(p: Nothing => Boolean): Boolean = true
    final def exists(p: Nothing => Boolean): Boolean = false
    private[iteratee] final val normalize: Input[Nothing] = this
    private[iteratee] final val toVector: Vector[Nothing] = Vector.empty
  }

  private[this] final val endValue: Input[Nothing] = new Input[Nothing] {
    final def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEnd
    final val isEmpty: Boolean = false
    final val isEnd: Boolean = true
    final def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    final def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    final def filter(f: Nothing => Boolean): Input[Nothing] = this
    final def foreach(f: Nothing => Unit): Unit = ()
    final def forall(p: Nothing => Boolean): Boolean = true
    final def exists(p: Nothing => Boolean): Boolean = false
    private[iteratee] final val normalize: Input[Nothing] = this
    private[iteratee] final val toVector: Vector[Nothing] = Vector.empty
  }
}

trait InputInstances extends VectorInstances {
  implicit final val input: Traverse[Input] with Monad[Input] =
    new Traverse[Input] with Monad[Input] {
      final def pure[A](a: A): Input[A] = Input.el(a)
      final def traverse[G[_], A, B](fa: Input[A])(f: A => G[B])(implicit
        G: Applicative[G]
      ): G[Input[B]] = fa.foldWith(
        new InputFolder[A, G[Input[B]]] {
          def onEmpty: G[Input[B]] = G.pure(Input.empty[B])
          def onEl(e: A): G[Input[B]] = G.map(f(e))(Input.el)
          def onChunk(es: Vector[A]): G[Input[B]] =
            G.map(Traverse[Vector].traverse[G, A, B](es)(f))(Input.chunk)
          def onEnd: G[Input[B]] = G.pure(Input.end[B])
        }
      )

      final def foldLeft[A, B](fa: Input[A], b: B)(f: (B, A) => B): B =
        fa.foldWith(
          new InputFolder[A, B] {
            def onEmpty: B = b
            def onEl(e: A): B = f(b, e)
            def onChunk(es: Vector[A]): B = Foldable[Vector].foldLeft(es, b)(f)
            def onEnd: B = b
          }
        )

      final def foldRight[A, B](fa: Input[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldWith(
          new InputFolder[A, Eval[B]] {
            def onEmpty: Eval[B] = b
            def onEl(e: A): Eval[B] = f(e, b)
            def onChunk(es: Vector[A]): Eval[B] = Foldable[Vector].foldRight(es, b)(f)
            def onEnd: Eval[B] = b
          }
        )

      override def filter_[A](fa: Input[A])(p: A => Boolean): List[A] = fa.filter(p).toVector.toList
      override def exists[A](fa: Input[A])(p: A => Boolean): Boolean = fa.exists(p)
      override def forall[A](fa: Input[A])(p: A => Boolean): Boolean = fa.forall(p)
      override def toList[A](fa: Input[A]): List[A] = fa.toVector.toList
      override def isEmpty[A](fa: Input[A]): Boolean = fa.isEmpty || fa.isEnd

      override def map[A, B](fa: Input[A])(f: A => B): Input[B] = fa.map(f)
      final def flatMap[A, B](fa: Input[A])(f: A => Input[B]): Input[B] = fa.flatMap(f)
    }

  implicit final def semigroupInput[A](implicit A: Semigroup[A]): Semigroup[Input[A]] =
    new Semigroup[Input[A]] {
      final def combine(a1: Input[A], a2: Input[A]): Input[A] = a1.normalize.foldWith(
        new InputFolder[A, Input[A]] {
          final def onEmpty: Input[A] = a2.normalize.foldWith(
            new InputFolder[A, Input[A]] {
              final def onEmpty: Input[A] = a2
              final def onEl(e: A): Input[A] = a2
              final def onChunk(es: Vector[A]): Input[A] = Input.el(es.reduceLeft(A.combine))
              final def onEnd: Input[A] = Input.end
            }
          )
          final def onEl(e: A): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              final def onEmpty: Input[A] = Input.el(e)
              final def onEl(f: A): Input[A] = Input.el(A.combine(e, f))
              final def onChunk(fs: Vector[A]): Input[A] = Input.el(fs.foldLeft(e)(A.combine))
              final def onEnd: Input[A] = Input.end
            }
          )
          final def onChunk(es: Vector[A]): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              final def onEmpty: Input[A] = Input.el(es.reduceLeft(A.combine))
              final def onEl(e: A): Input[A] = Input.el((es :+ e).reduceLeft(A.combine))
              final def onChunk(fs: Vector[A]): Input[A] =
                Input.el((es ++ fs).reduceLeft(A.combine))
              final def onEnd: Input[A] = Input.end
            }
          )
          final def onEnd: Input[A] = Input.end
        }
      )
    }

  implicit final def inputEq[A](implicit A: Eq[A]): Eq[Input[A]] =
    new Eq[Input[A]] {
      private[this] final lazy val eqVectorA: Eq[Vector[A]] = Eq[Vector[A]]

      final def eqv(a1: Input[A], a2: Input[A]): Boolean = a1.normalize.foldWith(
        new InputFolder[A, Boolean] {
          final def onEmpty: Boolean = a2.isEmpty
          final def onEl(e: A): Boolean = a2.exists(f => A.eqv(e, f))
          final def onChunk(es: Vector[A]): Boolean = a2.foldWith(
            new InputFolder[A, Boolean] {
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

  implicit final def inputShow[A](implicit A: Show[A]): Show[Input[A]] = 
    new Show[Input[A]] { self =>
      override final def show(f: Input[A]): String = f.foldWith(
        new InputFolder[A, String] {
          final def onEmpty: String = "empty"
          final def onEl(e: A): String = s"el(${ A.show(e) })"
          final def onChunk(es: Vector[A]): String =
            s"""chunk(${ es.map(A.show).mkString(", ") })"""
          final def onEnd: String = "end"
        }
      )
    }
}
