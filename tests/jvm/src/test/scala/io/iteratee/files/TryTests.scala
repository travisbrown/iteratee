package io.iteratee.files

import io.iteratee.{ Iteratee, Enumerator }
import io.iteratee.tests.BaseSuite
import scala.util.{ Success, Try }
import java.io.File

class TryTests extends BaseSuite {
  val tryModule = FileModule[Try]
  import tryModule._

  "readLines" should "enumerate text lines from a file" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readLines(txt).flatMap(line => Enumerator.enumVector(line.trim.split("\\s+").toVector))

    assert(enumerator.run(Iteratee.length) == Success(17973))
  }

  it should "work with an iteratee that stops early" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val result = "The Project Gutenberg EBook of Bartleby, The Scrivener, by Herman Melville"
    val enumerator = readLines(txt)

    assert(enumerator.run(Iteratee.head) == Success(Some(result)))
  }
}
