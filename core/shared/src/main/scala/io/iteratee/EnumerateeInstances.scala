package io.iteratee

import cats.Monad
import cats.arrow.Category
import cats.functor.Profunctor

trait EnumerateeInstances {
  implicit final def enumerateeInstance[F[_]](implicit F: Monad[F]):
    Category[({ type L[x, y] = Enumeratee[F, x, y]})#L] with
    Profunctor[({ type L[x, y] = Enumeratee[F, x, y]})#L] =
    new Category[({ type L[x, y] = Enumeratee[F, x, y]})#L] with
      Profunctor[({ type L[x, y] = Enumeratee[F, x, y]})#L] {
      final def id[A]: Enumeratee[F, A, A] = Enumeratee.map[F, A, A](identity)
      final def compose[A, B, C](
        f: Enumeratee[F, B, C],
        g: Enumeratee[F, A, B]
      ): Enumeratee[F, A, C] = g.andThen(f)

      def dimap[A, B, C, D](fab: Enumeratee[F, A, B])(f: C => A)(g: B => D): Enumeratee[F, C, D] =
        fab.map(g).contramap(f)
    }
}
