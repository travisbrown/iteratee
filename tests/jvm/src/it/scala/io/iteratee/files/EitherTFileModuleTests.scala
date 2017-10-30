package io.iteratee.files

import cats.Eval
import cats.data.EitherT
import io.iteratee.testing.files.FileModuleSuite
import io.iteratee.tests.EitherTSuite

class EitherTFileModuleTests extends FileModuleSuite[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
    with EitherTSuite with EitherTFileModule
