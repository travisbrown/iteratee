package io.iteratee

import cats.{ Eval, MonadError }
import cats.data.EitherT
import cats.effect.Sync
import cats.instances.either.catsStdInstancesForEither
import io.iteratee.internal.Step
import io.iteratee.modules.{ EitherModule, EitherTModule, FutureModule, TryModule }
import java.io.{
  BufferedInputStream,
  BufferedOutputStream,
  BufferedReader,
  BufferedWriter,
  File,
  FileInputStream,
  FileOutputStream,
  FileReader,
  FileWriter,
  InputStream,
  InputStreamReader,
  OutputStream,
  OutputStreamWriter
}
import java.util.zip.{ ZipEntry, ZipFile }
import scala.Predef.genericArrayOps
import scala.collection.JavaConverters._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

package object files {
  def future(implicit ec0: ExecutionContext): FutureModule with FileModule[Future] = new FutureFileModule {
    final protected def ec: ExecutionContext = ec0
  }

  def readLines[F[_]](file: File)(implicit F: Sync[F]): Enumerator[F, String] =
    enumerateLines(new BufferedReader(new FileReader(file)))

  def readLinesFromStream[F[_]](stream: InputStream)(implicit F: Sync[F]): Enumerator[F, String] =
    enumerateLines(new BufferedReader(new InputStreamReader(stream)))

  def readBytes[F[_]](file: File)(implicit F: Sync[F]): Enumerator[F, Array[Byte]] =
    enumerateBytes(new BufferedInputStream(new FileInputStream(file)))

  def readBytesFromStream[F[_]](stream: InputStream)(implicit F: Sync[F]): Enumerator[F, Array[Byte]] =
    enumerateBytes(new BufferedInputStream(stream))

  def readZipStreams[F[_]](file: File)(implicit F: Sync[F]): Enumerator[F, (ZipEntry, InputStream)] =
    Enumerator.liftM(F.delay(new ZipFile(file))).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala).ensure(F.delay(zipFile.close()))
    }

  def listFiles[F[_]](dir: File)(implicit F: Sync[F]): Enumerator[F, File] =
    Enumerator.liftM(F.delay(dir.listFiles)).flatMap {
      case null => Enumerator.empty[F, File]
      case files => Enumerator.enumVector(files.toVector)
    }

  def listFilesRec[F[_]](dir: File)(implicit F: Sync[F]): Enumerator[F, File] = listFiles[F](dir).flatMap {
    case item if item.isDirectory => listFilesRec(item)
    case item => Enumerator.enumOne(item)
  }

  def writeLines[F[_]](file: File)(implicit F: Sync[F]): Iteratee[F, String, Unit] =
    Iteratee.liftM(F.delay(new BufferedWriter(new FileWriter(file)))).flatMap { writer =>
      Iteratee.foldM[F, String, Unit](())((_, line) =>
        F.delay {
          writer.write(line)
          writer.newLine()
        }
      ).ensure(F.delay(writer.close()))
    }

  def writeLinesToStream[F[_]](stream: OutputStream)(implicit F: Sync[F]): Iteratee[F, String, Unit] =
    Iteratee.liftM(F.delay(new BufferedWriter(new OutputStreamWriter(stream)))).flatMap { writer =>
      Iteratee.foldM[F, String, Unit](())((_, line) =>
        F.delay {
          writer.write(line)
          writer.newLine()
        }
      ).ensure(F.delay(writer.close()))
    }

  def writeBytes[F[_]](file: File)(implicit F: Sync[F]): Iteratee[F, Array[Byte], Unit] =
    Iteratee.liftM(F.delay(new BufferedOutputStream(new FileOutputStream(file)))).flatMap { stream =>
      Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
        F.delay(stream.write(bytes))
      ).ensure(F.delay(stream.close()))
    }

  def writeBytesToStream[F[_]](stream: OutputStream)(implicit F: Sync[F]): Iteratee[F, Array[Byte], Unit] =
    Iteratee.liftM(F.delay(new BufferedOutputStream(stream))).flatMap { stream =>
      Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
        F.delay(stream.write(bytes))
      ).ensure(F.delay(stream.close()))
    }

  private[this] def enumerateLines[F[_]](reader: => BufferedReader)(implicit F: Sync[F]): Enumerator[F, String] =
    Enumerator.liftM(F.delay(reader)).flatMap(reader =>
      new LineEnumerator(reader).ensure(F.delay(reader.close()))
    )

  private[this] def enumerateBytes[F[_]](stream: => InputStream)(implicit F: Sync[F]): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(F.delay(stream)).flatMap(reader =>
      new ByteEnumerator(stream).ensure(F.delay(stream.close()))
    )

  private[this] final class LineEnumerator[F[_]](reader: BufferedReader)(implicit F: Sync[F])
      extends Enumerator[F, String] {
    final def apply[A](s: Step[F, String, A]): F[Step[F, String, A]] =
      if (s.isDone) F.pure(s) else F.flatMap(F.delay(reader.readLine())) {
        case null => F.pure(s)
        case line => F.flatMap(s.feedEl(line))(apply)
      }
  }

  private[this] final class ByteEnumerator[F[_]](stream: InputStream, bufferSize: Int = 8192)(implicit F: Sync[F])
      extends Enumerator[F, Array[Byte]] {
    final def apply[A](s: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] =
      if (s.isDone) F.pure(s) else F.flatten(
        F.delay {
          val array = new Array[Byte](bufferSize)
          val bytesRead = stream.read(array, 0, bufferSize)
          val read = if (bytesRead == bufferSize) array else array.slice(0, bytesRead)

          if (bytesRead == -1) F.pure(s) else F.flatMap(s.feedEl(read))(apply(_))
        }
      )
  }

  private[this] final class ZipFileEnumerator[F[_]](zipFile: ZipFile, iterator: Iterator[ZipEntry])(implicit F: Sync[F])
      extends Enumerator[F, (ZipEntry, InputStream)] {
    final def apply[A](s: Step[F, (ZipEntry, InputStream), A]): F[Step[F, (ZipEntry, InputStream), A]] =
      if (s.isDone) F.pure(s) else F.flatten(
        F.delay(
          if (iterator.hasNext) {
            val entry = iterator.next

            F.flatMap(s.feedEl((entry, zipFile.getInputStream(entry))))(apply)
          } else F.pure(s)
        )
      )
  }
}

package files {
  final object either extends EitherFileModule
  final object eitherT extends EitherTFileModule
  final object try_ extends TryFileModule

  trait EitherFileModule extends EitherModule with NonSuspendableFileModule[Either[Throwable, ?]]

  trait EitherTFileModule extends EitherTModule with SuspendableFileModule[EitherT[Eval, Throwable, ?]] {
    private[this] val E: MonadError[Either[Throwable, ?], Throwable] = catsStdInstancesForEither[Throwable]
    final protected def captureEffect[A](a: => A): EitherT[Eval, Throwable, A] =
      EitherT(Eval.always(E.catchNonFatal(a)))
  }

  trait FutureFileModule extends FutureModule with NonSuspendableFileModule[Future]
  trait TryFileModule extends TryModule with NonSuspendableFileModule[Try]
}
