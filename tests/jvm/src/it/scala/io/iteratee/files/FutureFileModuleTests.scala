package io.iteratee.files

import cats.instances.future._
import io.iteratee.testing.files.FileModuleSuite
import io.iteratee.tests.FutureSuite
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FutureFileModuleTests extends FileModuleSuite[Future] with FutureSuite with FutureFileModule
