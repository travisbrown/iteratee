package io.iteratee

import cats.MonadError
import java.io.{ BufferedReader, File, FileReader }
import scalaz.concurrent.Task

package object io extends Module[Task] {
  private[this] class LineEnumerator(reader: BufferedReader) extends Enumerator[Task, String] {
    final def apply[A](s: Step[Task, String, A]): Iteratee[Task, String, A] = s.foldWith(
      new MapContStepFolder[Task, String, A](s) {
        def onCont(k: Input[String] => Iteratee[Task, String, A]): Iteratee[Task, String, A] = {
          liftToIteratee(Task(reader.readLine())).flatMap {
            case null => s.pointI
            case line => k(Input.el(line)).advance(apply)
          }
        }
      }
    )
  }

  def lines(path: String): Enumerator[Task, String] =
    liftToEnumerator(Task(new BufferedReader(new FileReader(new File(path))))).flatMap { reader =>
      new LineEnumerator(reader).ensure(Task(reader.close()))
    }

  implicit val taskMonadError: MonadError[Task, Throwable] = new MonadError[Task, Throwable] {
    def pure[A](x: A): Task[A] = Task.taskInstance.point(x)
    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
    override def map[A, B](fa: Task[A])(f: A => B): Task[B] = fa.map(f)

    def raiseError[A](e: Throwable): Task[A] = Task.taskInstance.raiseError(e)
    def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] =
      Task.taskInstance.handleError(fa)(f)
  }
}
