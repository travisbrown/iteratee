package io.iteratee.files

import cats.MonadError
import io.iteratee.{ Enumerator, Iteratee }
import java.io.{ Closeable, File, InputStream, OutputStream }
import java.util.zip.ZipEntry
import scala.util.control.NonFatal

trait FileModule[F[_]] {
  def readLines(file: File): Enumerator[F, String]
  def readLinesFromStream(stream: InputStream): Enumerator[F, String]
  def readBytes(file: File): Enumerator[F, Array[Byte]]
  def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]]
  def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)]
  def listFiles(dir: File): Enumerator[F, File]
  def listFilesRec(dir: File): Enumerator[F, File]

  def writeLines(file: File): Iteratee[F, String, Unit]
  def writeLinesToStream(stream: OutputStream): Iteratee[F, String, Unit]
  def writeBytes(file: File): Iteratee[F, Array[Byte], Unit]
  def writeBytesToStream(stream: OutputStream): Iteratee[F, Array[Byte], Unit]

  protected final def bracket[R <: Closeable, A](fr: F[R])(f: R => F[A])(implicit F: MonadError[F, Throwable]): F[A] =
    F.flatMap(fr) { r =>
      F.handleErrorWith(f(r)) {
        case NonFatal(e) =>
          try r.close() catch {
            // We've already failed, so we ignore this exception.
            case NonFatal(_) => ()
          }
          F.raiseError(e)
      }
    }
}
