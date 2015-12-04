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
abstract class InputFolder[E, A] {
  def onChunk(es: Vector[E]): A
  def onEnd: A
  def onEmpty: A
  def onEl(e: E): A
}

/**
 * Input to an [[Iteratee]].
 */
sealed abstract class Input[E] { self =>
  /**
   * Reduce this [[Input]] to a value using the given four functions.
   */
  def foldWith[A](folder: InputFolder[E, A]): A

  def isEmpty: Boolean = false
  def isEnd: Boolean = false

  def map[X](f: E => X): Input[X]
  def flatMap[X](f: E => Input[X]): Input[X]
  def filter(f: E => Boolean): Input[E] = self
  def foreach(f: E => Unit): Unit = ()
  def forall(p: E => Boolean): Boolean = true
  def exists(p: E => Boolean): Boolean = false

  /**
   * Normalize the [[Input]] so that representations do not overlap.
   *
   * If this [[Input]] is a chunk with no values, an empty input will be returned, and if it's a
   * chunk with a single value, and element input will be returned.
   */
  private[iteratee] def normalize: Input[E] = self

  /**
   * Convert this [[Input]] value into a list of elements.
   */
  private[iteratee] def toVector: Vector[E] = Vector.empty

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
    override def map[X](f: E => X): Input[X] = Input.el(f(e))
    override def flatMap[X](f: E => Input[X]): Input[X] = f(e)
    override def filter(f: E => Boolean): Input[E] = if (f(e)) self else empty
    override def foreach(f: E => Unit): Unit = f(e)
    override def forall(p: E => Boolean): Boolean = p(e)
    override def exists(p: E => Boolean): Boolean = p(e)
    def foldWith[A](folder: InputFolder[E, A]): A = folder.onEl(e)
    override private[iteratee] def toVector: Vector[E] = Vector(e)
  }

  /**
   * An input value containing zero or more elements.
   */
  final def chunk[E](es: Vector[E]): Input[E] = new Input[E] { self =>
    override def isEmpty: Boolean = es.isEmpty
    override def map[X](f: E => X): Input[X] = chunk(es.map(f(_)))
    override def flatMap[X](f: E => Input[X]): Input[X] = es.foldLeft(empty[X]) {
      case (acc, _) if acc.isEnd => end
      case (acc, e) =>
        val ei = f(e)
        if (ei.isEnd) end else chunk(acc.toVector ++ ei.toVector)
    }
    override def filter(f: E => Boolean): Input[E] = Input.chunk(es.filter(f))
    override def foreach(f: E => Unit): Unit = es.foreach(f(_))
    override def forall(p: E => Boolean): Boolean = es.forall(p(_))
    override def exists(p: E => Boolean): Boolean = es.exists(p(_))
    def foldWith[A](folder: InputFolder[E, A]): A = folder.onChunk(es)

    override private[iteratee] def normalize: Input[E] = {
      val c = es.lengthCompare(1)
      if (c < 0) empty else if (c == 0) el(es.head) else self
    }

    override private[iteratee] def toVector: Vector[E] = es
  }

  private[this] final val emptyValue: Input[Nothing] = new Input[Nothing] {
    def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    override def isEmpty: Boolean = true
    def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEmpty
  }

  private[this] final val endValue: Input[Nothing] = new Input[Nothing] {
    def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    override def isEnd: Boolean = true
    def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEnd
  }
}

trait InputInstances extends VectorInstances {
  implicit val input: Traverse[Input] with Monad[Input] =
    new Traverse[Input] with Monad[Input] {
      def pure[A](a: A): Input[A] = Input.el(a)
      def traverse[G[_], A, B](fa: Input[A])(f: A => G[B])(implicit
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

      def foldLeft[A, B](fa: Input[A], b: B)(f: (B, A) => B): B =
        fa.foldWith(
          new InputFolder[A, B] {
            def onEmpty: B = b
            def onEl(e: A): B = f(b, e)
            def onChunk(es: Vector[A]): B = Foldable[Vector].foldLeft(es, b)(f)
            def onEnd: B = b
          }
        )

      def foldRight[A, B](fa: Input[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
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
      def flatMap[A, B](fa: Input[A])(f: A => Input[B]): Input[B] = fa.flatMap(f)
    }

  implicit def semigroupInput[A](implicit A: Semigroup[A]): Semigroup[Input[A]] =
    new Semigroup[Input[A]] {
      def combine(a1: Input[A], a2: Input[A]): Input[A] = a1.normalize.foldWith(
        new InputFolder[A, Input[A]] {
          def onEmpty: Input[A] = a2.normalize.foldWith(
            new InputFolder[A, Input[A]] {
              def onEmpty: Input[A] = a2
              def onEl(e: A): Input[A] = a2
              def onChunk(es: Vector[A]): Input[A] = Input.el(es.reduceLeft(A.combine))
              def onEnd: Input[A] = Input.end
            }
          )
          def onEl(e: A): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              def onEmpty: Input[A] = Input.el(e)
              def onEl(f: A): Input[A] = Input.el(A.combine(e, f))
              def onChunk(fs: Vector[A]): Input[A] = Input.el(fs.foldLeft(e)(A.combine))
              def onEnd: Input[A] = Input.end
            }
          )
          def onChunk(es: Vector[A]): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              def onEmpty: Input[A] = Input.el(es.reduceLeft(A.combine))
              def onEl(e: A): Input[A] = Input.el((es :+ e).reduceLeft(A.combine))
              def onChunk(fs: Vector[A]): Input[A] = Input.el((es ++ fs).reduceLeft(A.combine))
              def onEnd: Input[A] = Input.end
            }
          )
          def onEnd: Input[A] = Input.end
        }
      )
    }

  implicit def inputEq[A](implicit A: Eq[A]): Eq[Input[A]] =
    new Eq[Input[A]] {
      private[this] lazy val eqVectorA: Eq[Vector[A]] = Eq[Vector[A]]

      def eqv(a1: Input[A], a2: Input[A]): Boolean = a1.normalize.foldWith(
        new InputFolder[A, Boolean] {
          def onEmpty: Boolean = a2.isEmpty
          def onEl(e: A): Boolean = a2.exists(f => A.eqv(e, f))
          def onChunk(es: Vector[A]): Boolean = a2.foldWith(
            new InputFolder[A, Boolean] {
              def onEmpty: Boolean = false
              def onEl(e: A): Boolean = false
              def onChunk(fs: Vector[A]): Boolean = eqVectorA.eqv(es, fs)
              def onEnd: Boolean = false
            }
          )
          def onEnd: Boolean = a2.isEnd
        }
      )
    }

  implicit def inputShow[A](implicit A: Show[A]): Show[Input[A]] = 
    new Show[Input[A]] { self =>
      override def show(f: Input[A]): String = f.foldWith(
        new InputFolder[A, String] {
          def onEmpty: String = "empty"
          def onEl(e: A): String = s"el(${ A.show(e) })"
          def onChunk(es: Vector[A]): String = s"""chunk(${ es.map(A.show).mkString(", ") })"""
          def onEnd: String = "end"
        }
      )
    }
}
