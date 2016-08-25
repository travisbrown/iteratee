package io.iteratee.files

import cats.MonadError
import io.iteratee.{ Enumerator, Iteratee, Module }
import io.iteratee.internal.Step
import java.io.{
  BufferedInputStream,
  BufferedOutputStream,
  BufferedReader,
  BufferedWriter,
  Closeable,
  File,
  FileInputStream,
  FileOutputStream,
  FileReader,
  FileWriter,
  InputStream,
  InputStreamReader,
  OutputStream,
  OutputStreamWriter,
  Reader,
  Writer
}
import java.util.zip.{ ZipEntry, ZipFile }
import scala.Predef.genericArrayOps
import scala.collection.JavaConverters._

/**
 * File operations for contexts that are suspendable and for which recursive
 * monadic binding is stack safe.
 */
trait SuspendableFileModule[F[_]] extends FileModule[F] {
    this: Module[F] { type M[f[_]] <: MonadError[f, Throwable] } =>
  protected def captureEffect[A](a: => A): F[A]
  private[this] def close(c: Closeable): F[Unit] = captureEffect(c.close())

  private[this] def newFileReader(file: File): F[FileReader] = captureEffect(new FileReader(file))
  private[this] def newFileInputStream(file: File): F[FileInputStream] = captureEffect(new FileInputStream(file))

  private[this] def newInputStreamReader(stream: InputStream): F[InputStreamReader] =
    captureEffect(new InputStreamReader(stream))

  private[this] def newBufferedReader(reader: Reader): F[BufferedReader] = captureEffect(new BufferedReader(reader))

  private[this] def newBufferedInputStream(stream: InputStream): F[BufferedInputStream] =
    captureEffect(new BufferedInputStream(stream))

  private[this] def newFileWriter(file: File): F[FileWriter] = captureEffect(new FileWriter(file))
  private[this] def newFileOutputStream(file: File): F[FileOutputStream] = captureEffect(new FileOutputStream(file))

  private[this] def newOutputStreamWriter(stream: OutputStream): F[OutputStreamWriter] =
    captureEffect(new OutputStreamWriter(stream))

  private[this] def newBufferedWriter(writer: Writer): F[BufferedWriter] = captureEffect(new BufferedWriter(writer))

  private[this] def newBufferedOutputStream(stream: OutputStream): F[BufferedOutputStream] =
    captureEffect(new BufferedOutputStream(stream))

  final def readLines(file: File): Enumerator[F, String] = Enumerator.liftM(
    bracket(newFileReader(file))(newBufferedReader)(F)
  )(F).flatMap(reader => new LineEnumerator(reader).ensure(close(reader))(F))(F)

  final def readLinesFromStream(stream: InputStream): Enumerator[F, String] = Enumerator.liftM(
    bracket(newInputStreamReader(stream))(newBufferedReader)(F)
  )(F).flatMap(reader => new LineEnumerator(reader).ensure(close(reader))(F))(F)

  final def readBytes(file: File): Enumerator[F, Array[Byte]] = Enumerator.liftM(
    bracket(newFileInputStream(file))(newBufferedInputStream)(F)
  )(F).flatMap(stream => new ByteEnumerator(stream).ensure(close(stream))(F))(F)

  final def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]] = Enumerator.liftM(
    newBufferedInputStream(stream)
  )(F).flatMap(stream => new ByteEnumerator(stream).ensure(close(stream))(F))(F)

  final def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)] =
    Enumerator.liftM(captureEffect(new ZipFile(file)))(F).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala).ensure(close(zipFile))(F)
    }(F)

  final def listFiles(dir: File): Enumerator[F, File] =
    Enumerator.liftM(captureEffect(dir.listFiles))(F).flatMap {
      case null => Enumerator.empty[F, File](F)
      case files => Enumerator.enumVector(files.toVector)(F)
    }(F)

  final def listFilesRec(dir: File): Enumerator[F, File] = listFiles(dir).flatMap {
    case item if item.isDirectory => listFilesRec(item)
    case item => Enumerator.enumOne(item)(F)
  }(F)

  final def writeLines(file: File): Iteratee[F, String, Unit] = Iteratee.liftM(
    bracket(newFileWriter(file))(newBufferedWriter)(F)
  )(F).flatMap { writer =>
    Iteratee.foldM[F, String, Unit](())((_, line) =>
      captureEffect {
        writer.write(line)
        writer.newLine()
      }
    )(F).ensure(close(writer))(F)
  }(F)

  final def writeLinesToStream(stream: OutputStream): Iteratee[F, String, Unit] = Iteratee.liftM(
    bracket(newOutputStreamWriter(stream))(newBufferedWriter)(F)
  )(F).flatMap { writer =>
    Iteratee.foldM[F, String, Unit](())((_, line) =>
      captureEffect {
        writer.write(line)
        writer.newLine()
      }
    )(F).ensure(close(writer))(F)
  }(F)

  def writeBytes(file: File): Iteratee[F, Array[Byte], Unit] = Iteratee.liftM(
    bracket(newFileOutputStream(file))(newBufferedOutputStream)(F)
  )(F).flatMap { stream =>
    Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
      captureEffect(stream.write(bytes))
    )(F).ensure(close(stream))(F)
  }(F)

  def writeBytesToStream(stream: OutputStream): Iteratee[F, Array[Byte], Unit] = Iteratee.liftM(
    newBufferedOutputStream(stream)
  )(F).flatMap { stream =>
    Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
      captureEffect(stream.write(bytes))
    )(F).ensure(close(stream))(F)
  }(F)

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[F, String] {
    final def apply[A](s: Step[F, String, A]): F[Step[F, String, A]] =
      if (s.isDone) F.pure(s) else F.flatMap(captureEffect(reader.readLine())) {
        case null => F.pure(s)
        case line => F.flatMap(s.feedEl(line))(apply)
      }
  }

  private[this] final class ByteEnumerator(stream: InputStream, bufferSize: Int = 8192)
      extends Enumerator[F, Array[Byte]] {
    final def apply[A](s: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] =
      if (s.isDone) F.pure(s) else F.flatten(
        captureEffect {
          val array = new Array[Byte](bufferSize)
          val bytesRead = stream.read(array, 0, bufferSize)
          val read = if (bytesRead == bufferSize) array else array.slice(0, bytesRead)

          if (bytesRead == -1) F.pure(s) else F.flatMap(s.feedEl(read))(apply(_))
        }
      )
  }

  private[this] final class ZipFileEnumerator(zipFile: ZipFile, iterator: Iterator[ZipEntry])
      extends Enumerator[F, (ZipEntry, InputStream)] {
    final def apply[A](s: Step[F, (ZipEntry, InputStream), A]): F[Step[F, (ZipEntry, InputStream), A]] =
      if (s.isDone) F.pure(s) else F.flatten(
        captureEffect(
          if (iterator.hasNext) {
            val entry = iterator.next

            F.flatMap(s.feedEl((entry, zipFile.getInputStream(entry))))(apply)
          } else F.pure(s)
        )
      )
  }
}
