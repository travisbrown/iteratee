package io.iteratee.benchmark

import cats.Monad
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.free.Free
import io.iteratee.{Enumerator, Iteratee}
import java.io.InputStream
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.Predef.refArrayOps
import scala.util.Try

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/jmh:run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.FileModuleBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class FileModuleBenchmark {
  private def bartebly: InputStream = getClass.getResourceAsStream("/io/iteratee/examples/pg/11231/11231.txt")

  def linesIO: Enumerator[IO, String] = io.iteratee.files.modules.io.readLinesFromStream(bartebly)
  def linesTF: Enumerator[Free[Try, *], String] = FreeTryModule.readLinesFromStream(bartebly)

  def words[F[_]: Monad](line: String): Enumerator[F, String] = Enumerator.enumIndexedSeq(line.split(" ").toIndexedSeq)
  def avgLen[F[_]: Monad]: Iteratee[F, String, Double] = Iteratee
    .length[F, String]
    .zip(
      Iteratee.foldMap[F, String, Int](_.length)
    )
    .map {
      case (count, totalLengths) => (totalLengths.toDouble / count.toDouble)
    }

  @Benchmark
  def avgWordLengthIO: Double = linesIO.flatMap(words[IO]).into(avgLen).unsafeRunSync()

  @Benchmark
  def avgWordLengthTF: Double = linesTF.flatMap(words[Free[Try, *]]).into(avgLen[Free[Try, *]]).runTailRec.get
}
