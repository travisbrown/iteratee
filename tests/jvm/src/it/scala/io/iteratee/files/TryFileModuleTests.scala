package io.iteratee.files

import cats.instances.try_._
import io.iteratee.tests.TrySuite
import io.iteratee.tests.files.FileModuleSuite
import scala.util.Try

class TryFileModuleTests extends FileModuleSuite[Try] with TrySuite with TryFileModule
