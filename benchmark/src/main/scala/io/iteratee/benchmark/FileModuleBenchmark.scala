package io.iteratee.benchmark

import cats.Monad
import cats.free.Free
import cats.instances.int._
import com.twitter.util.{ Await => AwaitT, Duration => DurationT, Future => FutureT, Try => TryT }
import fs2.interop.cats._
import fs2.{ Task => TaskF }
import io.catbird.util._
import io.iteratee.{ Enumerator, Iteratee }
import io.iteratee.{ fs2 => f }
import io.iteratee.{ monix => m }
import io.iteratee.{ twitter => t }
import io.iteratee.{ scalaz => s }
import io.iteratee.scalaz.ScalazInstances
import java.io.File
import java.util.concurrent.TimeUnit
import monix.eval.{ Task => TaskM }
import org.openjdk.jmh.annotations._
import scala.Predef.refArrayOps
import scala.concurrent.Await
import scala.concurrent.duration._
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
  val linesM: Enumerator[TaskM, String] = m.task.readLines(bartebly)
  val linesF: Enumerator[TaskF, String] = f.task.readLines(bartebly)
  val linesTTF: Enumerator[Free[TryT, ?], String] = FreeTryModule.readLines(bartebly)

  def words[F[_]: Monad](line: String): Enumerator[F, String] = Enumerator.enumVector(line.split(" ").toVector)
  def avgLen[F[_]: Monad]: Iteratee[F, String, Double] = Iteratee.length[F, String].zip(
    Iteratee.foldMap[F, String, Int](_.length)
  ).map {
    case (count, totalLengths) => (totalLengths.toDouble / count.toDouble)
  }

  @Benchmark
  def avgWordLengthTR: Double = AwaitT.result(linesTR.flatMap(words[Rerunnable]).into(avgLen).run, DurationT.Top)

  @Benchmark
  def avgWordLengthTF: Double = AwaitT.result(linesTF.flatMap(words[FutureT]).into(avgLen), DurationT.Top)

  @Benchmark
  def avgWordLengthTT: Double = linesTT.flatMap(words[TryT]).into(avgLen).get

  @Benchmark
  def avgWordLengthS: Double = linesS.flatMap(words[Task]).into(avgLen).unsafePerformSync

  @Benchmark
  def avgWordLengthF: Double = {
    import m.task._

    Await.result(
      linesM.flatMap(words[TaskM]).into(avgLen).runAsync(monix.execution.Scheduler.Implicits.global),
      Duration.Inf
    )
  }

  @Benchmark
  def avgWordLengthM: Double = Await.result(
    linesF.flatMap(words[TaskF]).into(avgLen).unsafeRunAsyncFuture,
    Duration.Inf
  )

  @Benchmark
  def avgWordLengthTTF: Double = linesTTF.flatMap(words[Free[TryT, ?]]).into(avgLen[Free[TryT, ?]]).runTailRec.get
}
