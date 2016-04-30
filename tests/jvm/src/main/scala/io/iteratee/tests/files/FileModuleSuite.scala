package io.iteratee.tests.files

import cats.Monad
import io.iteratee.{ EnumeratorModule, IterateeModule, Module }
import io.iteratee.files.FileModule
import io.iteratee.tests.ModuleSuite
import java.io.File

abstract class FileModuleSuite[F[_]: Monad] extends ModuleSuite[F] {
  this: Module[F] with EnumeratorModule[F] with IterateeModule[F] with FileModule[F] =>

  test("readLines") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readLines(txt).flatMap(line => enumVector(line.trim.split("\\s+").toVector))

    assert(enumerator.run(length) === F.pure(17973))
  }

  test("readLines with head") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val result = "The Project Gutenberg EBook of Bartleby, The Scrivener, by Herman Melville"
    val enumerator = readLines(txt)

    assert(enumerator.run(head) === F.pure(Some(result)))
  }

  test("readBytes") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readBytes(txt).flatMap(bytes => enumVector(bytes.toVector))

    assert(enumerator.run(length) === F.pure(105397))
  }

  test("readZipStreams") {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)

    val enumerator = readZipStreams(zip).flatMap {
      case (_, stream) => readLinesFromStream(stream)
    }

    assert(enumerator.run(length) === F.pure(1981))
  }

  test("readZipStreams with head") {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)
    val enumerator = readZipStreams(zip).map(_._1.getName)

    assert(enumerator.run(head) === F.pure(Some("11231.txt")))
  }

  test("listFiles on directory") {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg/11231").toURI)
    val result = Vector("11231.txt", "11231.zip")
    val enumerator = listFiles(dir)

    assert(F.map(enumerator.toVector)(_.map(_.getName).sorted) === F.pure(result))
  }

  test("listFiles on file") {
    val notDir = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = listFiles(notDir)

    assert(F.map(enumerator.toVector)(_.isEmpty) === F.pure(true))
  }

  test("listFilesRec") {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg").toURI)
    val result = Vector("11231.txt", "11231.zip")
    val enumerator = listFilesRec(dir)

    assert(F.map(enumerator.toVector)(_.map(_.getName).sorted) === F.pure(result))
  }
}
