package io.iteratee.benchmark

import cats.Monad
import cats.instances.int._
import com.twitter.util.{ Await => AwaitT, Duration => DurationT, Future => FutureT, Try => TryT }
import io.catbird.util._
import io.iteratee.{ Enumerator, Iteratee }
import io.iteratee.{ twitter => t }
import io.iteratee.{ scalaz => s }
import io.iteratee.scalaz.ScalazInstances
import java.io.File
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.Predef.refArrayOps
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

  val linesTR: Enumerator[Rerunnable, String] = t.rerunnable.readLines(bartebly)
  val linesTF: Enumerator[FutureT, String] = t.future.readLines(bartebly)
  val linesTT: Enumerator[TryT, String] = t.try_.readLines(bartebly)
  val linesS: Enumerator[Task, String] = s.task.readLines(bartebly)
  val linesTTF: Enumerator[FreeTryModule.FreeTry, String] = FreeTryModule.readLines(bartebly)

  def words[F[_]: Monad](line: String): Enumerator[F, String] = Enumerator.enumVector(line.split(" ").toVector)
  def avgLen[F[_]: Monad]: Iteratee[F, String, Double] = Iteratee.length[F, String].zip(
    Iteratee.foldMap[F, String, Int](_.length)
  ).map {
    case (count, totalLengths) => (totalLengths.toDouble / count.toDouble)
  }

  @Benchmark
  def avgWordLengthTR: Double = AwaitT.result(linesTR.flatMap(words[Rerunnable]).run(avgLen).run, DurationT.Top)

  @Benchmark
  def avgWordLengthTF: Double = AwaitT.result(linesTF.flatMap(words[FutureT]).run(avgLen), DurationT.Top)

  @Benchmark
  def avgWordLengthTT: Double = linesTT.flatMap(words[TryT]).run(avgLen).get

  @Benchmark
  def avgWordLengthS: Double = linesS.flatMap(words[Task]).run(avgLen).unsafePerformSync

  @Benchmark
  def avgWordLengthTTF: Double = linesTTF.flatMap(words[FreeTryModule.FreeTry]).run(avgLen).runTailRec.get
}
