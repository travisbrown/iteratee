package io.iteratee.files

import cats.Eval
import cats.data.EitherT
import io.iteratee.tests.EitherTSuite
import io.iteratee.tests.files.FileModuleSuite

class EitherTFileModuleTests extends FileModuleSuite[({ type L[x] = EitherT[Eval, Throwable, x] })#L]
    with EitherTSuite with EitherTFileModule
