package io.iteratee.files

import cats.effect.{ IO, Sync }
import io.iteratee.modules.{ EnumerateeModule, EnumeratorErrorModule, IterateeErrorModule, Module }

package modules {
  final object io extends IOModule

  trait IOModule
      extends FileModule[IO]
      with Module[IO]
      with EnumerateeModule[IO]
      with EnumeratorErrorModule[IO, Throwable]
      with IterateeErrorModule[IO, Throwable] {
    type M[f[_]] = Sync[f]

    protected val F: Sync[IO] = IO.asyncForIO
  }
}
