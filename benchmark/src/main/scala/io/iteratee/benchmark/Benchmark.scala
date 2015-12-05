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

class InMemoryExampleData {
  val size = 10000
  val intsC: Vector[Int] = (0 until size).toVector
  val intsI: i.Enumerator[Int, Eval] = i.Enumerator.enumVector(intsC)
  val intsS: Process[Task, Int] = Process.emitAll(intsC)
  val intsZ: z.EnumeratorT[Int, Trampoline] = z.EnumeratorT.enumIndexedSeq(intsC)
}

class StreamingExampleData {
  val longStreamC: Stream[Long] = Stream.iterate(0L)(_ + 1L)
  val longStreamI: i.Enumerator[Long, Eval] = i.Enumerator.iterate[Long, Eval](0L)(_ + 1L)
  val longStreamS: Process[Task, Long] = Process.iterate(0L)(_ + 1L)
  val longStreamZ: z.EnumeratorT[Long, Trampoline] =
    z.EnumeratorT.iterate[Long, Trampoline](_ + 1L, 0L)
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.InMemoryBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class InMemoryBenchmark extends InMemoryExampleData {
  @Benchmark
  def sumIntsC: Int = intsC.sum

  @Benchmark
  def sumIntsI: Int = i.Iteratee.sum[Int, Eval].process(intsI).value

  @Benchmark
  def sumIntsS: Int = intsS.sum.runLastOr(sys.error("Impossible")).run

  @Benchmark
  def sumIntsZ: Int = (z.IterateeT.sum[Int, Trampoline] &= intsZ).run.run
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.StreamingBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StreamingBenchmark extends StreamingExampleData {
  val size = 10000

  @Benchmark
  def takeLongsC: Vector[Long] = longStreamC.take(size).toVector

  @Benchmark
  def takeLongsI: Vector[Long] = i.Iteratee.take[Long, Eval](size).process(longStreamI).value

  @Benchmark
  def takeLongsS: Vector[Long] = longStreamS.take(size).runLog.run

  @Benchmark
  def takeLongsZ: Vector[Long] =
    (z.Iteratee.take[Long, Vector](size).up[Trampoline] &= longStreamZ).run.run
}
