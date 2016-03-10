package io.iteratee.task

import io.iteratee.tests.BaseSuite
import java.io.File

class TaskOperationTest extends BaseSuite {
  test("lines") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)

    assert(lines(txt).flatMap(line => enumVector(line.trim.split("\\s+").toVector)).run(length).run === 17973)
  }

  test("lines with head") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val result = "The Project Gutenberg EBook of Bartleby, The Scrivener, by Herman Melville"

    assert(lines(txt).run(head).run === Some(result))
  }

  test("bytes") {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)

    assert(bytes(txt).flatMap(bytes => enumVector(bytes.toVector)).run(length).run === 105397)
  }

  test("zipStreams") {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)

    val enumerator = zipStreams(zip).flatMap {
      case (_, stream) => streamLines(stream)
    }

    assert(enumerator.run(length).run === 1981)
  }

  test("zipStreams with head") {
    val zip = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.zip").toURI)

    assert(zipStreams(zip).map(_._1.getName).run(head).run === Some("11231.txt"))
  }

  test("listContents on directory") {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg/11231").toURI)
    val result = Vector("11231.txt", "11231.zip")

    assert(listContents(dir).toVector.run.map(_.getName).sorted === result)
  }

  test("listContents on file") {
    val notDir = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)

    assert(listContents(notDir).toVector.run.isEmpty)
  }

  test("listAllFiles") {
    val dir = new File(getClass.getResource("/io/iteratee/examples/pg").toURI)
    val result = Vector("11231.txt", "11231.zip")

    assert(listAllFiles(dir).toVector.run.map(_.getName).sorted === result)
  }
}
