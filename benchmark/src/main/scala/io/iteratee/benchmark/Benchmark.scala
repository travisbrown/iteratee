package io.iteratee.benchmark

import cats.std.int._
import io.{ iteratee => i }
import io.iteratee.Module
import io.iteratee.task.TaskInstances
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import play.api.libs.{ iteratee => p }
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.concurrent.Task
import scalaz.{ iteratee => z }
import scalaz.std.anyVal.intInstance
import scalaz.std.vector._
import scalaz.stream.Process

class IterateeBenchmark extends Module[Task] with TaskInstances

class InMemoryExampleData extends IterateeBenchmark {
  private[this] val count = 10000

  val intsC: Vector[Int] = (0 until count).toVector
  val intsI: i.Enumerator[Task, Int] = enumVector(intsC)
  val intsS: Process[Task, Int] = Process.emitAll(intsC)
  val intsZ: z.EnumeratorT[Int, Task] = z.EnumeratorT.enumIndexedSeq(intsC)
  val intsP: p.Enumerator[Int] = p.Enumerator(intsC: _*)
}

class StreamingExampleData extends IterateeBenchmark {
  val longStreamI: i.Enumerator[Task, Long] = iterate(0L)(_ + 1L)
  val longStreamS: Process[Task, Long] = Process.iterate(0L)(_ + 1L)
  // scalaz-iteratee's iterate is broken.
  val longStreamZ: z.EnumeratorT[Long, Task] = z.EnumeratorT.repeat[Unit, Task](()).zipWithIndex.map(_._2)
  val longStreamP: p.Enumerator[Long] = p.Enumerator.unfold(0L)(i => Some((i + 1L, i)))
  val longStreamC: Stream[Long] = Stream.iterate(0L)(_ + 1L)
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/jmh:run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.InMemoryBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class InMemoryBenchmark extends InMemoryExampleData {
  @Benchmark
  def sumInts0I: Int = intsI.run(sum).run

  @Benchmark
  def sumInts1S: Int = intsS.sum.runLastOr(sys.error("Impossible")).run

  @Benchmark
  def sumInts2Z: Int = (z.IterateeT.sum[Int, Task] &= intsZ).run.run

  @Benchmark
  def sumInts3P: Int = Await.result(intsP.run(p.Iteratee.fold(0)(_ + _)), Duration.Inf)

  @Benchmark
  def sumInts4C: Int = intsC.sum
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/jmh:run -i 10 -wi 10 -f 2 -t 1 io.iteratee.benchmark.StreamingBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StreamingBenchmark extends StreamingExampleData {
  val count = 10000

  @Benchmark
  def takeLongs0I: Vector[Long] = longStreamI.run(take(count)).run

  @Benchmark
  def takeLongs1S: Vector[Long] = longStreamS.take(count).runLog.run

  @Benchmark
  def takeLongs2Z: Vector[Long] = (z.Iteratee.take[Long, Vector](count).up[Task] &= longStreamZ).run.run

  @Benchmark
  def takeLongs3P: Seq[Long] = Await.result(longStreamP.run(p.Iteratee.takeUpTo(count)), Duration.Inf)

  @Benchmark
  def takeLongs4C: Vector[Long] = longStreamC.take(count).toVector
}
