package io.iteratee.benchmark

import cats.Monad
import cats.effect.IO
import cats.free.Free
import cats.instances.int._
import cats.instances.try_._
import io.iteratee.{ Enumerator, Iteratee }
import io.iteratee.{ monix => m }
import io.iteratee.{ scalaz => s }
import io.iteratee.scalaz.ScalazInstances
import java.io.{ File, InputStream }
import java.util.concurrent.TimeUnit
import monix.eval.{ Task => TaskM }
import org.openjdk.jmh.annotations._
import scala.Predef.refArrayOps
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try
import scalaz.concurrent.Task

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
class FileModuleBenchmark extends ScalazInstances {
  private def bartebly: InputStream = getClass.getResourceAsStream("/io/iteratee/examples/pg/11231/11231.txt")

  def linesIO: Enumerator[IO, String] = io.iteratee.files.modules.io.readLinesFromStream(bartebly)
  def linesS: Enumerator[Task, String] = s.task.readLinesFromStream(bartebly)
  def linesM: Enumerator[TaskM, String] = m.task.readLinesFromStream(bartebly)
  def linesTF: Enumerator[Free[Try, ?], String] = FreeTryModule.readLinesFromStream(bartebly)

  def words[F[_]: Monad](line: String): Enumerator[F, String] = Enumerator.enumVector(line.split(" ").toVector)
  def avgLen[F[_]: Monad]: Iteratee[F, String, Double] = Iteratee.length[F, String].zip(
    Iteratee.foldMap[F, String, Int](_.length)
  ).map {
    case (count, totalLengths) => (totalLengths.toDouble / count.toDouble)
  }

  @Benchmark
  def avgWordLengthIO: Double = linesIO.flatMap(words[IO]).into(avgLen).unsafeRunSync

  @Benchmark
  def avgWordLengthS: Double = linesS.flatMap(words[Task]).into(avgLen).unsafePerformSync

  @Benchmark
  def avgWordLengthM: Double = {
    import m.task._

    Await.result(
      linesM.flatMap(words[TaskM]).into(avgLen).runAsync(monix.execution.Scheduler.Implicits.global),
      Duration.Inf
    )
  }

  @Benchmark
  def avgWordLengthTF: Double = linesTF.flatMap(words[Free[Try, ?]]).into(avgLen[Free[Try, ?]]).runTailRec.get
}
