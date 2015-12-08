package io.iteratee

import cats.MonadError
import _root_.io.circe.Json
import _root_.io.circe.jawn.CirceSupportParser
import jawn.AsyncParser

package object demo {
  def parser[F[_]](implicit F: MonadError[F, Throwable]): Enumeratee[F, String, Json] = {
    val parser = CirceSupportParser.async(mode = AsyncParser.UnwrapArray)

    def feed(parser: AsyncParser[Json])(in: String): F[Seq[Json]] =
      parser.absorb(in)(CirceSupportParser.facade).fold(F.raiseError, F.pure)

    Enumeratee.flatMap[F, String, Json](in =>
      Enumerator.liftM[F, Seq[Json]](feed(parser)(in)).flatMap(js =>
        Enumerator.enumVector[F, Json](js.toVector)
      )
    )
  }
}
