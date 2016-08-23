package io.iteratee.files

import cats.MonadError
import io.iteratee.{ Enumerator, Module }
import java.io.{ File, InputStream }
import java.util.zip.ZipEntry

trait FileModule[F[_]] { this: Module[F] { type M[f[_]] <: MonadError[f, Throwable] } =>
  protected def effectCapture: EffectCapture[F]

  private[this] val files = io.iteratee.files.`package`

  final def readLines(file: File): Enumerator[F, String] =
    files.readLines(file)(F, effectCapture)

  final def readLinesFromStream(stream: InputStream): Enumerator[F, String] =
    files.readLinesFromStream(stream)(F, effectCapture)

  final def readBytes(file: File): Enumerator[F, Array[Byte]] =
    files.readBytes(file)(F, effectCapture)

  final def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]] =
    files.readBytesFromStream(stream)(F, effectCapture)

  final def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)] =
    files.readZipStreams(file)(F, effectCapture)

  final def listFiles(dir: File): Enumerator[F, File] = files.listFiles(dir)(F, effectCapture)
  final def listFilesRec(dir: File): Enumerator[F, File] = files.listFilesRec(dir)(F, effectCapture)
}

object FileModule {
  private[this] class FromMonadError[F[_]](monadError: MonadError[F, Throwable]) extends Module[F] with FileModule[F] {
    type M[F[T]] = MonadError[F, Throwable]
    val F: MonadError[F, Throwable] = monadError
    protected def effectCapture: EffectCapture[F] = EffectCapture.fromApplicativeError(monadError)
  }

  /**
   * Create a [[FileModule]] using the default implementation of `captureEffect`.
   */
  def apply[F[_]](implicit monadError: MonadError[F, Throwable]): FileModule[F] =
    new FromMonadError[F](monadError)
}
