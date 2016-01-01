package io.iteratee.task

import io.iteratee.Enumerator
import io.iteratee.internal.{ MapContStepFolder, Step }
import java.io.{ BufferedReader, File, FileReader }
import scalaz.concurrent.Task

/**
 * @groupname Task Task operations and instances
 * @groupprio Task -1
 */
trait TaskOperations {
  /**
   * @group Task
   */
  final def lines(path: String): Enumerator[Task, String] =
    Enumerator.liftM(Task(new BufferedReader(new FileReader(new File(path))))).flatMap { reader =>
      new LineEnumerator(reader).ensure(Task(reader.close()))
    }

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[Task, String] {
    final def apply[A](s: Step[Task, String, A]): Task[Step[Task, String, A]] = s.foldWith(
      new MapContStepFolder[Task, String, A](s) {
        def onCont(k: List[String] => Task[Step[Task, String, A]]): Task[Step[Task, String, A]] = {
          Task(reader.readLine()).flatMap {
            case null => Task.taskInstance.point(s)
            case line => k(List(line)).flatMap(apply)
          }
        }
      }
    )
  }
}
