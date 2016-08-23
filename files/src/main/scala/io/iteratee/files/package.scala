package io.iteratee

import cats.{ Eval, MonadError }
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
import scala.util.{ Left, Right }

package object files {
  final object xor extends XorFileModule

  final def readLines[F[_]](file: File)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, String] =
    Enumerator.liftMEval(
      Eval.always(C(new BufferedReader(new FileReader(file))))
    ).flatMap { reader =>
      new LineEnumerator(reader).ensureEval(Eval.later(C(reader.close())))
    }

  final def readLinesFromStream[F[_]](stream: InputStream)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, String] =
    Enumerator.liftM(
      C(new BufferedReader(new InputStreamReader(stream)))
    ).flatMap { reader =>
      new LineEnumerator(reader).ensureEval(Eval.later(C(reader.close())))
    }

  final def readBytes[F[_]](file: File)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, Array[Byte]] =
    Enumerator.liftMEval(
      Eval.always(C(new BufferedInputStream(new FileInputStream(file))))
    ).flatMap { stream =>
      new ByteEnumerator(stream).ensureEval(Eval.later(C(stream.close())))
    }

  final def readBytesFromStream[F[_]](stream: InputStream)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(C(new BufferedInputStream(stream))).flatMap { stream =>
      new ByteEnumerator(stream).ensureEval(Eval.later(C(stream.close())))
    }

  final def readZipStreams[F[_]](file: File)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, (ZipEntry, InputStream)] =
    Enumerator.liftMEval(Eval.always(C(new ZipFile(file)))).flatMap { zipFile =>
      new ZipFileEnumerator(zipFile, zipFile.entries.asScala)
        .ensureEval(Eval.later(C(zipFile.close())))
    }

  final def listFiles[F[_]](dir: File)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, File] =
    Enumerator.liftMEval(Eval.always(C(dir.listFiles))).flatMap {
      case null => Enumerator.empty[F, File]
      case files => Enumerator.enumVector(files.toVector)
    }

  final def listFilesRec[F[_]](dir: File)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ): Enumerator[F, File] = listFiles(dir).flatMap {
    case item if item.isDirectory => listFilesRec(item)
    case item => Enumerator.enumOne(item)
  }

  private[this] final class LineEnumerator[F[_]](reader: BufferedReader)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ) extends Enumerator[F, String] {
    final def apply[A](s: Step[F, String, A]): F[Step[F, String, A]] = F.tailRecM(s) { step =>
      if (step.isDone) F.pure(Right(step)) else F.flatMap(C(reader.readLine())) {
        case null => F.pure(Right(step))
        case line => F.map(step.feedEl(line))(Left(_))
      }
    }
  }

  private[this] final class ByteEnumerator[F[_]](stream: InputStream, bufferSize: Int = 8192)(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ) extends Enumerator[F, Array[Byte]] {
    final def apply[A](s: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] = F.tailRecM(s) { step =>
      if (step.isDone) F.pure(Right(step)) else F.flatten(
        C {
          val array = new Array[Byte](bufferSize)
          val bytesRead = stream.read(array, 0, bufferSize)
          val read = if (bytesRead == bufferSize) array else array.slice(0, bytesRead)

          if (bytesRead == -1) F.pure(Right(step)) else F.map(step.feedEl(read))(Left(_))
        }
      )
    }
  }

  private[this] final class ZipFileEnumerator[F[_]](zipFile: ZipFile, iterator: Iterator[ZipEntry])(implicit
    F: MonadError[F, Throwable],
    C: EffectCapture[F]
  ) extends Enumerator[F, (ZipEntry, InputStream)] {
    final def apply[A](s: Step[F, (ZipEntry, InputStream), A]): F[Step[F, (ZipEntry, InputStream), A]] =
      F.tailRecM(s) { step =>
        if (step.isDone) F.pure(Right(step)) else F.flatten(
          C(
            if (iterator.hasNext) {
              val entry = iterator.next

              F.map(step.feedEl((entry, zipFile.getInputStream(entry))))(Left(_))
            } else F.pure(Right(step))
          )
        )
      }
  }
}
