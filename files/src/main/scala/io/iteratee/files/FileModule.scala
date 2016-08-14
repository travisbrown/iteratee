package io.iteratee.files

import cats.MonadError
import cats.data.Xor
import io.iteratee.{ Enumerator, Module }
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
import scala.Predef.genericArrayOps
import scala.collection.JavaConverters._

trait FileModule[F[_]] { this: Module[F] { type M[f[_]] <: MonadError[f, Throwable] } =>
  protected def captureEffect[A](a: => A): F[A]

  final def readLines(file: File): Enumerator[F, String] =
    Enumerator.liftM(captureEffect(new BufferedReader(new FileReader(file))))(F).flatMap { reader =>
      new LineEnumerator(reader).ensure(captureEffect(reader.close()))(F)
    }(F)

  final def readLinesFromStream(stream: InputStream): Enumerator[F, String] =
    Enumerator.liftM(captureEffect(new BufferedReader(new InputStreamReader(stream))))(F).flatMap { reader =>
      new LineEnumerator(reader).ensure(captureEffect(reader.close()))(F)
    }(F)

  final def readBytes(file: File): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(captureEffect(new BufferedInputStream(new FileInputStream(file))))(F).flatMap { stream =>
      new ByteEnumerator(stream).ensure(captureEffect(stream.close()))(F)
    }(F)

  final def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(captureEffect(new BufferedInputStream(stream)))(F).flatMap { stream =>
      new ByteEnumerator(stream).ensure(captureEffect(stream.close()))(F)
    }(F)

  final def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)] =
    Enumerator.liftM(captureEffect(new ZipFile(file)))(F).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala).ensure(captureEffect(zipFile.close()))(F)
    }(F)

  final def listFiles(dir: File): Enumerator[F, File] = Enumerator.liftM(captureEffect(dir.listFiles))(F).flatMap {
    case null => Enumerator.empty[F, File](F)
    case files => Enumerator.enumVector(files.toVector)(F)
  }(F)

  final def listFilesRec(dir: File): Enumerator[F, File] = listFiles(dir).flatMap {
    case item if item.isDirectory => listFilesRec(item)
    case item => Enumerator.enumOne(item)(F)
  }(F)

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[F, String] {
    final def apply[A](s: Step[F, String, A]): F[Step[F, String, A]] = F.tailRecM(s) { step =>
      if (step.isDone) F.pure(Xor.right(step)) else F.flatMap(captureEffect(reader.readLine())) {
        case null => F.pure(Xor.right(step))
        case line => F.map(step.feedEl(line))(Xor.left)
      }
    }
  }

  private[this] final class ByteEnumerator(stream: InputStream, bufferSize: Int = 8192)
    extends Enumerator[F, Array[Byte]] {
    final def apply[A](step: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] =
      if (step.isDone) F.pure(step) else F.flatten(
        captureEffect {
          val array = new Array[Byte](bufferSize)
          val read = stream.read(array, 0, bufferSize)

          if (read == -1) F.pure(step) else F.flatMap(step.feedEl(array.slice(0, read)))(apply(_))
        }
      )
  }

  private[this] final class ZipFileEnumerator(zipFile: ZipFile, iterator: Iterator[ZipEntry])
    extends Enumerator[F, (ZipEntry, InputStream)] {
    final def apply[A](step: Step[F, (ZipEntry, InputStream), A]): F[Step[F, (ZipEntry, InputStream), A]] =
      if (step.isDone) F.pure(step) else F.flatten(
        captureEffect(
          if (iterator.hasNext) {
            val entry = iterator.next

            F.flatMap(step.feedEl((entry, zipFile.getInputStream(entry))))(apply)
          } else F.pure(step)
        )
      )
  }
}
