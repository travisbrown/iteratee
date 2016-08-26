package io.iteratee.files

import cats.Eval
import cats.data.XorT
import io.iteratee.tests.XorSuite
import io.iteratee.tests.files.FileModuleSuite

class XorFileModuleTests extends FileModuleSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L]
    with XorSuite with XorFileModule
