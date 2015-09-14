package io.iteratee

import algebra.Eq
import cats.Monad
import cats.laws.discipline.EqK
import cats.std.vector._

trait EqInstances {
  implicit def eqEnumerator[A: Eq, F[_]: Monad](implicit F: EqK[F]): Eq[Enumerator[A, F]] =
    F.synthesize[Vector[A]].on[Enumerator[A, F]](_.drainTo)
}
