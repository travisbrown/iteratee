package io.iteratee.benchmark

import cats.Monad
import cats.free.Free
import cats.instances.future._
import cats.instances.int._
import cats.instances.try_._
import io.iteratee.{ Enumerator, Iteratee }
import io.iteratee.{ monix => m }
import io.iteratee.{ scalaz => s }
import io.iteratee.scalaz.ScalazInstances
import java.io.File
import java.util.concurrent.TimeUnit
import monix.eval.{ Task => TaskM }
import org.openjdk.jmh.annotations._
import scala.Predef.refArrayOps
import scala.concurrent.{ Await, Future }
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
  val bartebly = new File(getClass.getResource("/io/iteratee/examples/pg/11231/11231.txt").toURI)

  val linesF: Enumerator[Future, String] = io.iteratee.files.future.readLines(bartebly)
  val linesT: Enumerator[Try, String] = io.iteratee.files.try_.readLines(bartebly)
  val linesS: Enumerator[Task, String] = s.task.readLines(bartebly)
  val linesM: Enumerator[TaskM, String] = m.task.readLines(bartebly)
  val linesTF: Enumerator[Free[Try, ?], String] = FreeTryModule.readLines(bartebly)

  def words[F[_]: Monad](line: String): Enumerator[F, String] = Enumerator.enumVector(line.split(" ").toVector)
  def avgLen[F[_]: Monad]: Iteratee[F, String, Double] = Iteratee.length[F, String].zip(
    Iteratee.foldMap[F, String, Int](_.length)
  ).map {
    case (count, totalLengths) => (totalLengths.toDouble / count.toDouble)
  }

  @Benchmark
  def avgWordLengthF: Double = Await.result(linesF.flatMap(words[Future]).into(avgLen), Duration.Inf)

  @Benchmark
  def avgWordLengthT: Double = linesT.flatMap(words[Try]).into(avgLen).get

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
