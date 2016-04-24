package io.iteratee.files

import cats.Eval
import cats.data.XorT
import io.iteratee.tests.XorSuite
import io.iteratee.tests.files.FileModuleSuite

class XorFileModuleSuite extends FileModuleSuite[({ type L[x] = XorT[Eval, Throwable, x] })#L] with XorFileModule
  with XorSuite
