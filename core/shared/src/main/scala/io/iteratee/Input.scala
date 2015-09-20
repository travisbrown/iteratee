package io.iteratee

import algebra.{ Eq, Semigroup }
import cats.{ Applicative, Eval, Foldable, Monad, SemigroupK, Show, Traverse }
import cats.std.VectorInstances

abstract class InputFolder[E, A] {
  def onEmpty: A
  def onEl(e: E): A
  def onChunk(es: Seq[E]): A
  def onEof: A
}

/**The input to an iteratee. **/
sealed abstract class Input[E] { self =>
  def foldX[Z](chunk: (=> Seq[E]) => Z, eof: => Z): Z = foldWith[Z](
    new InputFolder[E, Z] {
      def onEmpty: Z = chunk(Seq.empty)
      def onEl(e: E): Z = chunk(Seq(e))
      def onChunk(es: Seq[E]): Z = chunk(es)
      def onEof: Z = eof
    }
  )

  def foldWith[A](folder: InputFolder[E, A]): A
  def normalize: Input[E] = self
  def isEmpty: Boolean = false
  def isEl: Boolean = false
  def isChunk: Boolean = false
  def isEof: Boolean = false
  def map[X](f: E => X): Input[X]
  def flatMap[X](f: E => Input[X]): Input[X]
  def filter(f: E => Boolean): Input[E] = self
  def foreach(f: E => Unit): Unit = ()
  def forall(p: E => Boolean): Boolean = true
  def exists(p: E => Boolean): Boolean = false
  def values: Seq[E] = Nil

  override final def toString = foldWith(
    new InputFolder[E, String] {
      def onEmpty: String = "Empty"
      def onEl(e: E): String = e.toString
      def onChunk(es: Seq[E]): String = es.map(_.toString).mkString(", ")
      def onEof: String = "EOF"
    }
  )
}

object Input extends InputInstances {
  def empty[E]: Input[E] = emptyValue.asInstanceOf[Input[E]]

  def el[E](e: E): Input[E] = new Input[E] { self =>
    override def isEl: Boolean = true
    override def map[X](f: E => X): Input[X] = Input.el(f(e))
    override def flatMap[X](f: E => Input[X]): Input[X] = f(e)
    override def filter(f: E => Boolean): Input[E] = if (f(e)) self else Input.empty
    override def foreach(f: E => Unit): Unit = f(e)
    override def forall(p: E => Boolean): Boolean = p(e)
    override def exists(p: E => Boolean): Boolean = p(e)
    override def values: Seq[E] = Vector(e)
    def foldWith[A](folder: InputFolder[E, A]): A = folder.onEl(e)
  }

  def chunk[E](es: Seq[E]): Input[E] = new Input[E] { self =>
    override def normalize: Input[E] = {
      val c = es.lengthCompare(1)
      if (c < 0) Input.empty else if (c == 0) Input.el(es(0)) else self
    }
    override def isChunk: Boolean = true
    override def isEmpty: Boolean = es.isEmpty
    override def map[X](f: E => X): Input[X] = Input.chunk(es.map(f(_)))
    override def flatMap[X](f: E => Input[X]): Input[X] =
      es.foldLeft(Input.empty[X]) {
        case (acc, e) =>
          val ei = f(e)

          if (acc.isEof || ei.isEof) Input.eof else {
            Input.chunk(acc.values ++ ei.values)
          }
      }
    override def filter(f: E => Boolean): Input[E] = Input.chunk(es.filter(f))
    override def foreach(f: E => Unit): Unit = es.foreach(f(_))
    override def forall(p: E => Boolean): Boolean = es.forall(p(_))
    override def exists(p: E => Boolean): Boolean = es.exists(p(_))
    override def values: Seq[E] = es
    def foldWith[A](folder: InputFolder[E, A]): A = folder.onChunk(es)
  }

  def eof[E]: Input[E] = eofValue.asInstanceOf[Input[E]]

  private[this] val emptyValue: Input[Nothing] = new Input[Nothing] {
    def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    override def isEmpty: Boolean = true
    def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEmpty
  }

  private[this] val eofValue: Input[Nothing] = new Input[Nothing] {
    def map[X](f: Nothing => X): Input[X] = this.asInstanceOf[Input[X]]
    def flatMap[X](f: Nothing => Input[X]): Input[X] = this.asInstanceOf[Input[X]]
    override def isEof: Boolean = true
    def foldWith[A](folder: InputFolder[Nothing, A]): A = folder.onEof
  }
}

sealed abstract class InputInstances extends VectorInstances {
  implicit val input: Traverse[Input] with Monad[Input] =
    new Traverse[Input] with Monad[Input] {
      def pure[A](a: A): Input[A] = Input.el(a)
      def traverse[G[_]: Applicative, A, B](fa: Input[A])(f: A => G[B]): G[Input[B]] =
        fa.foldWith(
          new InputFolder[A, G[Input[B]]] {
            def onEmpty: G[Input[B]] = Applicative[G].pure(Input.empty[B])
            def onEl(e: A): G[Input[B]] = Applicative[G].map(f(e))(Input.el)
            def onChunk(es: Seq[A]): G[Input[B]] =
              Applicative[G].map(Traverse[Vector].traverse[G, A, B](es.toVector)(f))(Input.chunk)
            def onEof: G[Input[B]] = Applicative[G].pure(Input.eof[B])
          }
        )

      def foldLeft[A, B](fa: Input[A], z: B)(f: (B, A) => B): B =
        fa.foldWith(
          new InputFolder[A, B] {
            def onEmpty: B = z
            def onEl(e: A): B = f(z, e)
            def onChunk(es: Seq[A]): B = Foldable[Vector].foldLeft(es.toVector, z)(f)
            def onEof: B = z
          }
        )

      def foldRight[A, B](fa: Input[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldWith(
          new InputFolder[A, Eval[B]] {
            def onEmpty: Eval[B] = z
            def onEl(e: A): Eval[B] = f(e, z)
            def onChunk(es: Seq[A]): Eval[B] = Foldable[Vector].foldRight(es.toVector, z)(f)
            def onEof: Eval[B] = z
          }
        )

      override def filter_[A](fa: Input[A])(p: A => Boolean): List[A] = fa.filter(p).values.toList
      override def exists[A](fa: Input[A])(p: A => Boolean): Boolean = fa.exists(p)
      override def forall[A](fa: Input[A])(p: A => Boolean): Boolean = fa.forall(p)
      override def toList[A](fa: Input[A]): List[A] = fa.values.toList
      override def isEmpty[A](fa: Input[A]): Boolean = fa.isEmpty || fa.isEof

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
              def onChunk(es: Seq[A]): Input[A] = Input.el(es.reduceLeft(A.combine))
              def onEof: Input[A] = Input.eof
            }
          )
          def onEl(e: A): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              def onEmpty: Input[A] = Input.el(e)
              def onEl(f: A): Input[A] = Input.el(A.combine(e, f))
              def onChunk(fs: Seq[A]): Input[A] = Input.el(fs.foldLeft(e)(A.combine))
              def onEof: Input[A] = Input.eof
            }
          )
          def onChunk(es: Seq[A]): Input[A] = a2.foldWith(
            new InputFolder[A, Input[A]] {
              def onEmpty: Input[A] = Input.el(es.reduceLeft(A.combine))
              def onEl(e: A): Input[A] = Input.el((es :+ e).reduceLeft(A.combine))
              def onChunk(fs: Seq[A]): Input[A] = Input.el((es ++ fs).reduceLeft(A.combine))
              def onEof: Input[A] = Input.eof
            }
          )
          def onEof: Input[A] = Input.eof
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
          def onChunk(es: Seq[A]): Boolean = a2.foldWith(
            new InputFolder[A, Boolean] {
              def onEmpty: Boolean = false
              def onEl(e: A): Boolean = false
              def onChunk(fs: Seq[A]): Boolean = eqVectorA.eqv(es.toVector, fs.toVector)
              def onEof: Boolean = false
            }
          )
          def onEof: Boolean = a2.isEof
        }
      )
    }

  implicit def inputShow[A](implicit A: Show[A]): Show[Input[A]] = 
    new Show[Input[A]] { self =>
      override def show(f: Input[A]): String = f.foldWith(
        new InputFolder[A, String] {
          def onEmpty: String = "empty-input"
          def onEl(e: A): String = s"el-input(${ A.show(e) })"
          def onChunk(es: Seq[A]): String = s"""el-chunk(${ es.map(A.show).mkString(", ") })"""
          def onEof: String = "eof-input"
        }
      )
    }
}
