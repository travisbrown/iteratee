package io.iteratee.files

import io.iteratee.Enumerator
import java.io.{ File, InputStream }
import java.util.zip.ZipEntry

trait FileModule[F[_]] {
  def readLines(file: File): Enumerator[F, String]
  def readLinesFromStream(stream: InputStream): Enumerator[F, String]
  def readBytes(file: File): Enumerator[F, Array[Byte]]
  def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]]
  def readZipStreams(file: File): Enumerator[F, (ZipEntry, InputStream)]
  def listFiles(dir: File): Enumerator[F, File]
  def listFilesRec(dir: File): Enumerator[F, File]
}
