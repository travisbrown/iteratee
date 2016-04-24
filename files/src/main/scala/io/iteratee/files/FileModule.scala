package io.iteratee.files

import cats.MonadError
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

trait FileModule[F[_]] {
  protected def fileModuleF: MonadError[F, Throwable]
  protected def captureEffect[A](a: => A): F[A]

  def readLines(file: File): Enumerator[F, String] =
    Enumerator.liftM(captureEffect(new BufferedReader(new FileReader(file))))(fileModuleF).flatMap { reader =>
      new LineEnumerator(reader).ensure(captureEffect(reader.close()))(fileModuleF)
    }(fileModuleF)

  def readLinesFromStream(stream: InputStream): Enumerator[F, String] =
    Enumerator.liftM(captureEffect(new BufferedReader(new InputStreamReader(stream))))(fileModuleF).flatMap { reader =>
      new LineEnumerator(reader).ensure(captureEffect(reader.close()))(fileModuleF)
    }(fileModuleF)

  def readBytes(file: File): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(captureEffect(new BufferedInputStream(new FileInputStream(file))))(fileModuleF).flatMap { stream =>
      new ByteEnumerator(stream).ensure(captureEffect(stream.close()))(fileModuleF)
    }(fileModuleF)

  def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)] =
    Enumerator.liftM(captureEffect(new ZipFile(file)))(fileModuleF).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala).ensure(captureEffect(zipFile.close()))(fileModuleF)
    }(fileModuleF)

  def listFiles(dir: File): Enumerator[F, File] = Enumerator.liftM(captureEffect(dir.listFiles))(fileModuleF).flatMap {
    case null => Enumerator.empty[F, File](fileModuleF)
    case files => Enumerator.enumVector(files.toVector)(fileModuleF)
  }(fileModuleF)

  def listFilesRec(dir: File): Enumerator[F, File] = listFiles(dir).flatMap {
    case item if item.isDirectory => listFilesRec(item)
    case item => Enumerator.enumOne(item)(fileModuleF)
  }(fileModuleF)

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[F, String] {
    final def apply[A](step: Step[F, String, A]): F[Step[F, String, A]] =
      if (step.isDone) fileModuleF.pure(step) else fileModuleF.flatMap(captureEffect(reader.readLine())) {
        case null => fileModuleF.pure(step)
        case line => fileModuleF.flatMap(step.feedEl(line))(apply)
      }
  }

  private[this] final class ByteEnumerator(stream: InputStream, bufferSize: Int = 8092)
    extends Enumerator[F, Array[Byte]] {
    final def apply[A](step: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] =
      if (step.isDone) fileModuleF.pure(step) else fileModuleF.flatten(
        captureEffect {
          val array = new Array[Byte](bufferSize)
          val read = stream.read(array, 0, bufferSize)

          if (read == -1) fileModuleF.pure(step) else fileModuleF.flatMap(step.feedEl(array.slice(0, read)))(apply)
        }
      )
  }

  private[this] final class ZipFileEnumerator(zipFile: ZipFile, iterator: Iterator[ZipEntry])
    extends Enumerator[F, (ZipEntry, InputStream)] {
    final def apply[A](step: Step[F, (ZipEntry, InputStream), A]): F[Step[F, (ZipEntry, InputStream), A]] =
      if (step.isDone) fileModuleF.pure(step) else fileModuleF.flatten(
        captureEffect(
          if (iterator.hasNext) {
            val entry = iterator.next

            fileModuleF.flatMap(step.feedEl((entry, zipFile.getInputStream(entry))))(apply)
          } else fileModuleF.pure(step)
        )
      )
  }
}
