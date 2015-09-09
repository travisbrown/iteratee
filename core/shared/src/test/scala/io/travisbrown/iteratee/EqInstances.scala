package io.travisbrown.iteratee

import algebra.Eq
import cats.Monad
import cats.laws.discipline.EqK
import cats.std.list._

trait EqInstances {
  implicit def eqEnumeratorT[A: Eq, F[_]: Monad](implicit F: EqK[F]): Eq[EnumeratorT[A, F]] =
    F.synthesize[List[A]].on[EnumeratorT[A, F]](_.drainTo[List])
}
