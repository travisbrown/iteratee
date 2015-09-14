package io.iteratee

import cats.Applicative

abstract class StepFolder[E, F[_], A, B] {
  def onCont(f: Input[E] => Iteratee[E, F, A]): B
  def onDone(value: A, remainder: Input[E]): B
}

/**
 * The current state of an Iteratee, one of:
 *  - '''cont''' Waiting for more data
 *  - '''done''' Already calculated a result
 *
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[cats.Id]] is used to model pure computations, and is fixed as such in the type alias [[Step]].
 * @tparam A The type of the calculated result
 */
sealed abstract class Step[E, F[_], A] {
  private[iteratee] def unsafeValue: A

  def fold[B](
    cont: (Input[E] => Iteratee[E, F, A]) => B,
    done: (=> A, => Input[E]) => B
  ): B = foldWith(
    new StepFolder[E, F, A, B] {
      def onCont(k: Input[E] => Iteratee[E, F, A]): B = cont(k)
      def onDone(value: A, remainder: Input[E]): B = done(value, remainder)
    }
  )

  def foldWith[B](folder: StepFolder[E, F, A, B]): B

  def isCont: Boolean
  def isDone: Boolean

  def mapContOr[Z](k: (Input[E] => Iteratee[E, F, A]) => Z, z: Z): Z

  def mapCont(
    k: (Input[E] => Iteratee[E, F, A]) => Iteratee[E, F, A]
  )(implicit F: Applicative[F]): Iteratee[E, F, A] =
    mapContOr[Iteratee[E, F, A]](k, pointI)

  def pointI(implicit F: Applicative[F]): Iteratee[E, F, A] = new Iteratee(F.pure(this))
}

// object Step is in the implicit scope for Enumerator, so we mix in EnumeratorInstances here.
object Step extends EnumeratorInstances {
  def cont[E, F[_], A](c: Input[E] => Iteratee[E, F, A]): Step[E, F, A] =
    new Step[E, F, A] {
      private[iteratee] def unsafeValue: A = sys.error("diverging iteratee")
      def isCont: Boolean = true
      def isDone: Boolean = false
      def foldWith[B](folder: StepFolder[E, F, A, B]): B = folder.onCont(c)
      def mapContOr[Z](k: (Input[E] => Iteratee[E, F, A]) => Z, z: Z): Z = k(c)
    }

  def done[E, F[_], A](d: A, r: Input[E]): Step[E, F, A] =
    new Step[E, F, A] {
      private[iteratee] def unsafeValue: A = d
      def isCont: Boolean = false
      def isDone: Boolean = true
      def foldWith[B](folder: StepFolder[E, F, A, B]): B = folder.onDone(d, r)
      def mapContOr[Z](k: (Input[E] => Iteratee[E, F, A]) => Z, z: Z): Z = z
    }
}
