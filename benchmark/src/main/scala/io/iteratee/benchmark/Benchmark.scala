package io.iteratee.benchmark

import cats.Eval
import cats.std.int._
import cats.std.list.{ listAlgebra, listInstance => listInstanceC }
import io.{ iteratee => i }
import io.iteratee.task._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.concurrent.Task
import scalaz.{ iteratee => z }
import scalaz.std.anyVal.intInstance
import scalaz.std.list.{ listInstance => listInstanceS, listMonoid }
import scalaz.std.vector._
import scalaz.stream.Process

class InMemoryExampleData {
  val size = 10000
  val intsC: Vector[Int] = (0 until size).toVector
  val intsI: i.Enumerator[Task, Int] = i.Enumerator.enumVector(intsC)
  val intsS: Process[Task, Int] = Process.emitAll(intsC)
  val intsZ: z.EnumeratorT[Int, Task] = z.EnumeratorT.enumIndexedSeq(intsC)
}

class StreamingExampleData {
  val longStreamC: Stream[Long] = Stream.iterate(0L)(_ + 1L)
  val longStreamI: i.Enumerator[Task, Long] = i.Enumerator.iterate[Task, Long](0L)(_ + 1L)
  val longStreamS: Process[Task, Long] = Process.iterate(0L)(_ + 1L)
  val longStreamZ: z.EnumeratorT[Long, Task] = z.EnumeratorT.iterate[Long, Task](_ + 1L, 0L)
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
  def sumInts0: Int = intsI.run(i.Iteratee.sum[Task, Int]).run

  @Benchmark
  def sumInts1: Int = intsS.sum.runLastOr(sys.error("Impossible")).run

  @Benchmark
  def sumInts2: Int = (z.IterateeT.sum[Int, Task] &= intsZ).run.run

  @Benchmark
  def sumInts3: Int = intsC.sum
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
  def takeLongs0: Vector[Long] = longStreamI.run(i.Iteratee.take[Task, Long](size)).run

  @Benchmark
  def takeLongs1: Vector[Long] = longStreamS.take(size).runLog.run

  @Benchmark
  def takeLongs2: Vector[Long] =
    (z.Iteratee.take[Long, Vector](size).up[Task] &= longStreamZ).run.run

  @Benchmark
  def takeLongs3: Vector[Long] = longStreamC.take(size).toVector
}
