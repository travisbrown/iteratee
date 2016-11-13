package io.iteratee

import cats.Monad
import cats.arrow.Category
import cats.functor.Profunctor

private[iteratee] trait EnumerateeInstances {
  implicit final def enumerateeInstance[F[_]](implicit F: Monad[F]):
    Category[Enumeratee[F, ?, ?]] with
    Profunctor[Enumeratee[F, ?, ?]] =
    new Category[Enumeratee[F, ?, ?]] with Profunctor[Enumeratee[F, ?, ?]] {
      final def id[A]: Enumeratee[F, A, A] = Enumeratee.identity[F, A]
      final def compose[A, B, C](f: Enumeratee[F, B, C], g: Enumeratee[F, A, B]): Enumeratee[F, A, C] = g.andThen(f)
      final def dimap[A, B, C, D](fab: Enumeratee[F, A, B])(f: C => A)(g: B => D): Enumeratee[F, C, D] =
        fab.map(g).contramap(f)
    }
}
