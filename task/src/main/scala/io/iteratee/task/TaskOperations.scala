package io.iteratee.task

import cats.data.NonEmptyVector
import io.iteratee.Enumerator
import io.iteratee.internal.Step
import java.io.{
  BufferedInputStream,
  BufferedReader,
  File,
  FileInputStream,
  FileReader,
  InputStream,
  InputStreamReader
}
import java.util.zip.{ ZipEntry, ZipFile }
import scala.collection.JavaConverters._
import scalaz.concurrent.Task

/**
 * @groupname Task Task operations and instances
 * @groupprio Task -1
 */
trait TaskOperations {
  /**
   * @group Task
   */
  final def lines(file: File): Enumerator[Task, String] =
    Enumerator.liftM(Task(new BufferedReader(new FileReader(file)))).flatMap { reader =>
      new LineEnumerator(reader).ensure(Task(reader.close()))
    }

  /**
   * @group Task
   */
  final def streamLines(stream: InputStream): Enumerator[Task, String] =
    Enumerator.liftM(Task(new BufferedReader(new InputStreamReader(stream)))).flatMap { reader =>
      new LineEnumerator(reader).ensure(Task(reader.close()))
    }

  /**
   * @group Task
   */
  final def bytes(file: File): Enumerator[Task, Array[Byte]] =
    Enumerator.liftM(Task(new BufferedInputStream(new FileInputStream(file)))).flatMap { stream =>
      new ByteEnumerator(stream).ensure(Task(stream.close()))
    }

  /**
   * @group Task
   */
  final def zipStreams(file: File): Enumerator[Task, (ZipEntry, InputStream)] =
    Enumerator.liftM(Task(new ZipFile(file))).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala).ensure(Task(zipFile.close()))
    }

  /**
   * @group Task
   */
  final def listContents(dir: File): Enumerator[Task, File] =
    Enumerator.liftM(Task(dir.listFiles)).flatMap {
      case null => Enumerator.empty
      case files => Enumerator.enumVector(files.toVector)
    }

  /**
   * @group Task
   */
  final def listAllFiles(dir: File): Enumerator[Task, File] = listContents(dir).flatMap {
    case item if item.isDirectory => listAllFiles(item)
    case item => Enumerator.enumOne(item)
  }

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[Task, String] {
    final def apply[A](step: Step[Task, String, A]): Task[Step[Task, String, A]] =
      if (step.isDone) Task.taskInstance.point(step) else Task(reader.readLine()).flatMap {
        case null => Task.taskInstance.point(step)
        case line => step.feedEl(line).flatMap(apply)
      }
  }

  private[this] final class ByteEnumerator(stream: InputStream, bufferSize: Int = 8092)
    extends Enumerator[Task, Array[Byte]] {
    final def apply[A](step: Step[Task, Array[Byte], A]): Task[Step[Task, Array[Byte], A]] =
      if (step.isDone) Task.taskInstance.point(step) else Task {
        val array = new Array[Byte](bufferSize)
        val read = stream.read(array, 0, bufferSize)

        if (read == -1) None else Some(array.slice(0, read))
      }.flatMap {
        case None => Task.taskInstance.point(step)
        case Some(bytes) => step.feedEl(bytes).flatMap(apply)
      }
  }

  private[this] final class ZipFileEnumerator(zipFile: ZipFile, iterator: Iterator[ZipEntry])
    extends Enumerator[Task, (ZipEntry, InputStream)] {
    final def apply[A](step: Step[Task, (ZipEntry, InputStream), A]): Task[Step[Task, (ZipEntry, InputStream), A]] =
      if (step.isDone) Task.taskInstance.point(step) else Task(
        if (iterator.hasNext) {
          val entry = iterator.next
          (entry, zipFile.getInputStream(entry))
        } else null
      ).flatMap {
        case null => Task.taskInstance.point(step)
        case pair => step.feedEl(pair).flatMap(apply)
      }
  }
}
