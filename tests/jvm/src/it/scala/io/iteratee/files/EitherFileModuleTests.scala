package io.iteratee.files

import cats.instances.either._
import io.iteratee.tests.EitherSuite
import io.iteratee.tests.files.FileModuleSuite

class EitherFileModuleTests extends FileModuleSuite[({ type L[x] = Either[Throwable, x] })#L] with EitherSuite
    with EitherFileModule
