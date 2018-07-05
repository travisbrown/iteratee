package io.iteratee.files.modules

import cats.effect.Sync
import _root_.io.iteratee.{ Enumerator, Iteratee }
import _root_.io.iteratee.modules.Module
import java.io.{ File, InputStream, OutputStream }
import java.util.zip.ZipEntry

trait FileModule[F[_]] { this: Module[F] { type M[f[_]] = Sync[f] } =>
  final def readLines(file: File): Enumerator[F, String] = _root_.io.iteratee.files.readLines[F](file)(F)
  final def readLinesFromStream(stream: InputStream): Enumerator[F, String] =
    _root_.io.iteratee.files.readLinesFromStream[F](stream)(F)
  final def readBytes(file: File): Enumerator[F, Array[Byte]] = _root_.io.iteratee.files.readBytes[F](file)(F)
  final def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]] =
    _root_.io.iteratee.files.readBytesFromStream[F](stream)(F)
  final def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)] =
    _root_.io.iteratee.files.readZipStreams[F](file)(F)
  final def listFiles(dir: File): Enumerator[F, File] = _root_.io.iteratee.files.listFiles[F](dir)(F)
  final def listFilesRec(dir: File): Enumerator[F, File] = _root_.io.iteratee.files.listFilesRec[F](dir)(F)
  final def writeLines(file: File): Iteratee[F, String, Unit] = _root_.io.iteratee.files.writeLines[F](file)(F)
  final def writeLinesToStream(stream: OutputStream): Iteratee[F, String, Unit] =
    _root_.io.iteratee.files.writeLinesToStream[F](stream)(F)
  final def writeBytes(file: File): Iteratee[F, Array[Byte], Unit] =
    _root_.io.iteratee.files.writeBytes[F](file)(F)
  final def writeBytesToStream(stream: OutputStream): Iteratee[F, Array[Byte], Unit] =
    _root_.io.iteratee.files.writeBytesToStream[F](stream)(F)
}
