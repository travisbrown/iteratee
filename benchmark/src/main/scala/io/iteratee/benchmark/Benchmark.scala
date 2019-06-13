package io.iteratee.benchmark

import cats.Id
import cats.effect.IO
import cats.instances.int._
import fs2.{ Stream => StreamF }
import io.{ iteratee => i }
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.Predef.intWrapper
import scalaz.concurrent.Task
import scalaz.{ iteratee => z }
import scalaz.std.anyVal.intInstance
import scalaz.std.vector._

class InMemoryExampleData {
  private[this] val count = 10000

  val intsC: Vector[Int] = (0 until count).toVector
  val intsII: i.Enumerator[Id, Int] = i.Enumerator.enumVector[Id, Int](intsC)
  val intsIO: i.Enumerator[IO, Int] = i.Enumerator.enumVector[IO, Int](intsC)
  val intsZ: z.EnumeratorT[Int, Task] = z.EnumeratorT.enumIndexedSeq(intsC)
  val intsF: StreamF[IO, Int] = StreamF.emits(intsC)
}

class StreamingExampleData {
  val longStreamII: i.Enumerator[Id, Long] = i.Enumerator.iterate[Id, Long](0L)(_ + 1L)
  val longStreamIO: i.Enumerator[IO, Long] = i.Enumerator.StackUnsafe.iterate[IO, Long](0L)(_ + 1L)
  // scalaz-iteratee's iterate is broken.
  val longStreamZ: z.EnumeratorT[Long, Task] = z.EnumeratorT.repeat[Unit, Task](()).zipWithIndex.map(_._2)
  val longStreamF: StreamF[IO, Long] = StreamF.iterate(0L)(_ + 1L)
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
  def sumInts0II: Int = intsII.into(i.Iteratee.sum)

  @Benchmark
  def sumInts1IO: Int = intsIO.into(i.Iteratee.sum).unsafeRunSync

  @Benchmark
  def sumInts2Z: Int = (z.IterateeT.sum[Int, Task] &= intsZ).run.unsafePerformSync

  @Benchmark
  def sumInts3F: Int = intsF.fold1(_ + _).compile.last.unsafeRunSync.get

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
  def takeLongs0II: Vector[Long] = longStreamII.into(i.Iteratee.take(count))

  @Benchmark
  def takeLongs1IO: Vector[Long] = longStreamIO.into(i.Iteratee.take(count)).unsafeRunSync

  @Benchmark
  def takeLongs2Z: Vector[Long] = (z.Iteratee.take[Long, Vector](count).up[Task] &= longStreamZ).run.unsafePerformSync

  @Benchmark
  def takeLongs3F: Vector[Long] = longStreamF.take(count.toLong).compile.toVector.unsafeRunSync

  @Benchmark
  def takeLongs4C: Vector[Long] = longStreamC.take(count).toVector
}
