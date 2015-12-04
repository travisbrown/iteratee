package io.iteratee.benchmark

import cats.Eval
import cats.std.int._
import cats.std.list.{ listAlgebra, listInstance => listInstanceC }
import io.{ iteratee => i }
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.Free.Trampoline
import scalaz.concurrent.Task
import scalaz.{ iteratee => z }
import scalaz.std.anyVal.intInstance
import scalaz.std.list.{ listInstance => listInstanceS, listMonoid }
import scalaz.std.vector._
import scalaz.stream.Process

class ExampleData {
  val maxSize = 10000
  val intsC: Stream[Int] = Stream.from(0).take(maxSize)
  val intsI: i.Enumerator[Int, Eval] = i.Enumerator.enumStream(Stream.from(0).take(maxSize))
  val intsZ: z.EnumeratorT[Int, Trampoline] = z.EnumeratorT.enumStream(Stream.from(0).take(maxSize))
  val intsS: Process[Task, Int] = Process.range(0, maxSize)

  val longsC: Stream[Long] = Stream.iterate(0L)(_ + 1L)
  val longsI: i.Enumerator[Long, Eval] = i.Enumerator.iterate[Long, Eval](0L)(_ + 1L)
  val longsZ: z.EnumeratorT[Long, Trampoline] = z.EnumeratorT.iterate[Long, Trampoline](_ + 1L, 0L)
  val longsS: Process[Task, Long] = Process.iterate(0L)(_ + 1L)
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.IterateeBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IterateeBenchmark extends ExampleData {
  @Benchmark
  def sumIntsI: Int = i.Iteratee.sum[Int, Eval].process(intsI).value

  @Benchmark
  def sumIntsC: Int = intsC.sum

  @Benchmark
  def sumIntsZ: Int = (z.IterateeT.sum[Int, Trampoline] &= intsZ).run.run

  @Benchmark
  def sumIntsS: Int = intsS.sum.runLastOr(sys.error("Impossible")).run

  @Benchmark
  def takeLongsI: Vector[Long] = i.Iteratee.take[Long, Eval](10000).process(longsI).value

  @Benchmark
  def takeLongsC: Vector[Long] = longsC.take(maxSize).toVector

  @Benchmark
  def takeLongsZ: Vector[Long] =
    (z.Iteratee.take[Long, Vector](maxSize).up[Trampoline] &= longsZ).run.run

  @Benchmark
  def takeLongsS: Vector[Long] = longsS.take(maxSize).runLog.run
}
