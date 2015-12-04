package io.iteratee

import algebra.Order
import cats.Monad

trait EnumerateeModule[F[_]] {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  final def map[O, I](f: O => I)(implicit F: Monad[F]): Enumeratee[O, I, F] =
    Enumeratee.map[O, I, F](f)

  final def flatMap[O, I](f: O => Enumerator[I, F])(implicit F: Monad[F]): Enumeratee[O, I, F] =
    Enumeratee.flatMap[O, I, F](f)

  final def collect[O, I](pf: PartialFunction[O, I])(implicit F: Monad[F]): Enumeratee[O, I, F] =
    Enumeratee.collect[O, I, F](pf)

  final def filter[E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[E, E, F] =
    Enumeratee.filter[E, F](p)

  final def sequenceI[O, I](iteratee: Iteratee[O, F, I])(implicit
    F: Monad[F]
  ): Enumeratee[O, I, F] =
    Enumeratee.sequenceI[O, I, F](iteratee)

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  final def uniq[E: Order](implicit F: Monad[F]): Enumeratee[E, E, F] = Enumeratee.uniq[E, F]
    
  /**
   * Zip with the count of elements that have been encountered.
   */
  final def zipWithIndex[E](implicit F: Monad[F]): Enumeratee[E, (E, Long), F] =
    Enumeratee.zipWithIndex[E, F]

  final def grouped[E](n: Int)(implicit F: Monad[F]): Enumeratee[E, Vector[E], F] =
    Enumeratee.grouped[E, F](n)

  final def splitOn[E](p: E => Boolean)(implicit F: Monad[F]): Enumeratee[E, Vector[E], F] =
    Enumeratee.splitOn[E, F](p)

  final def cross[E1, E2](e2: Enumerator[E2, F])(implicit
    F: Monad[F]
  ): Enumeratee[E1, (E1, E2), F] =
    Enumeratee.cross[E1, E2, F](e2)
}
