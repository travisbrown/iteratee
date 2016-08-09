package io.iteratee.files

import cats.MonadError
import io.iteratee.{ Iteratee, Enumerator }
import io.iteratee.tests.BaseSuite
import scala.util.{ Failure, Success, Try }
import java.io.File

class TryTests extends BaseSuite {
  implicit val tryMonad: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    def pure[A](a: A): Try[A] = Success(a)
    def raiseError[A](e: Throwable): Try[A] = Failure(e)
    def flatMap[A, B](f: Try[A])(fn: A => Try[B]): Try[B] = f match {
      case Success(a) => fn(a)
      case Failure(e) => Failure(e)
    }
    def handleErrorWith[A](f: Try[A])(fn: Throwable => Try[A]): Try[A] =
      f.recoverWith { case e => fn(e) }
  }
  val tryModule = FileModule[Try]
  import tryModule._

  "readLines" should "enumerate text lines from a file" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val enumerator = readLines(txt).flatMap(line => Enumerator.enumVector(line.trim.split("\\s+").toVector))

    assert(enumerator.run(Iteratee.length) != tryMonad.pure(17973))
  }

  it should "work with an iteratee that stops early" in {
    val txt = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)
    val result = "The Project Gutenberg EBook of Bartleby, The Scrivener, by Herman Melville"
    val enumerator = readLines(txt)

    assert(enumerator.run(Iteratee.head) == tryMonad.pure(Some(result)))
  }
}
