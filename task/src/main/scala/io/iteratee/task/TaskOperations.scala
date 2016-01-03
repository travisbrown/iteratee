package io.iteratee.task

import io.iteratee.Enumerator
import io.iteratee.internal.{ Input, Step }
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
    final def apply[A](step: Step[Task, String, A]): Task[Step[Task, String, A]] =
      if (step.isDone) Task.taskInstance.point(step) else Task(reader.readLine()).flatMap {
        case null => Task.taskInstance.point(step)
        case line => step.feed(Input.el(line)).flatMap(apply)
      }
  }
}
