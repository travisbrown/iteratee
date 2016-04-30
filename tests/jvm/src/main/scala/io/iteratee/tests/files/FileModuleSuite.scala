package io.iteratee.tests.files

import cats.Monad
import io.iteratee.{ EnumeratorModule, IterateeModule, Module }
import io.iteratee.files.FileModule
import io.iteratee.tests.ModuleSuite
import java.io.File

abstract class FileModuleSuite[F[_]: Monad] extends ModuleSuite[F] {
  this: Module[F] with EnumeratorModule[F] with IterateeModule[F] with FileModule[F] =>

  "readLines" should "enumerate text lines from a file" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readLines(txt).flatMap(line => enumVector(line.trim.split("\\s+").toVector))

    assert(enumerator.run(length) === F.pure(17973))
  }

  it should "work with an iteratee that stops early" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val result = "The Project Gutenberg EBook of Bartleby, The Scrivener, by Herman Melville"
    val enumerator = readLines(txt)

    assert(enumerator.run(head) === F.pure(Some(result)))
  }

  "readBytes" should "enumerate bytes from a file" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readBytes(txt).flatMap(bytes => enumVector(bytes.toVector))

    assert(enumerator.run(length) === F.pure(105397))
  }

  "readZipStreams" should "enumerate files in a zip archive" in {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)

    val enumerator = readZipStreams(zip).flatMap {
      case (_, stream) => readLinesFromStream(stream)
    }

    assert(enumerator.run(length) === F.pure(1981))
  }

  it should "work with an iteratee that stops early" in {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)
    val enumerator = readZipStreams(zip).map(_._1.getName)

    assert(enumerator.run(head) === F.pure(Some("11231.txt")))
  }

  "listFiles" should "enumerate files in a directory" in {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg/11231").toURI)
    val result = Vector("11231.txt", "11231.zip")
    val enumerator = listFiles(dir)

    assert(F.map(enumerator.toVector)(_.map(_.getName).sorted) === F.pure(result))
  }

  it should "fail properly on a file" in {
    val notDir = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = listFiles(notDir)

    assert(F.map(enumerator.toVector)(_.isEmpty) === F.pure(true))
  }

  "listFilesRec" should "enumerate files in a directory recursively" in {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg").toURI)
    val result = Vector("11231.txt", "11231.zip")
    val enumerator = listFilesRec(dir)

    assert(F.map(enumerator.toVector)(_.map(_.getName).sorted) === F.pure(result))
  }
}
