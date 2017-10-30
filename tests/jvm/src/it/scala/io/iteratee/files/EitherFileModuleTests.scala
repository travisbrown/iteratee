package io.iteratee.files

import cats.instances.either._
import io.iteratee.testing.files.FileModuleSuite
import io.iteratee.tests.EitherSuite

class EitherFileModuleTests extends FileModuleSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
    with EitherFileModule
