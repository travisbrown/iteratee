package io.iteratee.files

import cats.instances.try_._
import io.iteratee.testing.files.FileModuleSuite
import io.iteratee.tests.TrySuite
import scala.util.Try

class TryFileModuleTests extends FileModuleSuite[Try] with TrySuite with TryFileModule
