package io.iteratee.benchmark

import cats.instances.int._
import com.twitter.util.{ Await => AwaitT, Duration => DurationT }
import io.catbird.util.Rerunnable
import io.{ iteratee => i }
import io.iteratee.scalaz.ScalazInstances
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import play.api.libs.{ iteratee => p }
import scala.Predef.intWrapper
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.concurrent.Task
import scalaz.{ iteratee => z }
import scalaz.std.anyVal.intInstance
import scalaz.std.vector._
import scalaz.stream.Process

class IterateeBenchmark extends ScalazInstances

class InMemoryExampleData extends IterateeBenchmark {
  private[this] val count = 10000

  val intsC: Vector[Int] = (0 until count).toVector
  val intsI: i.Enumerator[Task, Int] = i.Enumerator.enumVector[Task, Int](intsC)
  val intsR: i.Enumerator[Rerunnable, Int] = i.Enumerator.enumVector[Rerunnable, Int](intsC)
  val intsS: Process[Task, Int] = Process.emitAll(intsC)
  val intsZ: z.EnumeratorT[Int, Task] = z.EnumeratorT.enumIndexedSeq(intsC)
  val intsP: p.Enumerator[Int] = p.Enumerator(intsC: _*)
  val intsF: fs2.Stream[fs2.Task, Int] = fs2.Stream.emits(intsC)
}

class StreamingExampleData extends IterateeBenchmark {
  val longStreamI: i.Enumerator[Task, Long] = i.Enumerator.iterate[Task, Long](0L)(_ + 1L)
  val longStreamR: i.Enumerator[Rerunnable, Long] = i.Enumerator.iterate[Rerunnable, Long](0L)(_ + 1L)
  val longStreamS: Process[Task, Long] = Process.iterate(0L)(_ + 1L)
  // scalaz-iteratee's iterate is broken.
  val longStreamZ: z.EnumeratorT[Long, Task] = z.EnumeratorT.repeat[Unit, Task](()).zipWithIndex.map(_._2)
  val longStreamP: p.Enumerator[Long] = p.Enumerator.unfold(0L)(i => Some((i + 1L, i)))
  val longStreamC: Stream[Long] = Stream.iterate(0L)(_ + 1L)
  val longStreamF: fs2.Stream[fs2.Task, Long] = {
    // fs2 doesn't have an iterate yet.
    def iterate[A](start: A)(f: A => A): fs2.Stream[Nothing, A] = {
      fs2.Stream.emit(start) ++ iterate(f(start))(f)
    }
    iterate(0L)(_ + 1L)
  }
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
  def sumInts0IS: Int = intsI.into(i.Iteratee.sum).unsafePerformSync

  @Benchmark
  def sumInts1IR: Int = AwaitT.result(intsR.into(i.Iteratee.sum).run, DurationT.Top)

  @Benchmark
  def sumInts3S: Int = intsS.sum.runLastOr(sys.error("Impossible")).unsafePerformSync

  @Benchmark
  def sumInts4Z: Int = (z.IterateeT.sum[Int, Task] &= intsZ).run.unsafePerformSync

  @Benchmark
  def sumInts5P: Int = Await.result(intsP.run(p.Iteratee.fold(0)(_ + _)), Duration.Inf)

  @Benchmark
  def sumInts6C: Int = intsC.sum

  @Benchmark
  def sumInts7F: Int = intsF.sum.runLast.unsafeRun.get
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
  def takeLongs0IS: Vector[Long] = longStreamI.into(i.Iteratee.take(count)).unsafePerformSync

  @Benchmark
  def takeLongs1IR: Vector[Long] = AwaitT.result(longStreamR.into(i.Iteratee.take(count)).run, DurationT.Top)

  @Benchmark
  def takeLongs3S: Vector[Long] = longStreamS.take(count).runLog.unsafePerformSync

  @Benchmark
  def takeLongs4Z: Vector[Long] = (z.Iteratee.take[Long, Vector](count).up[Task] &= longStreamZ).run.unsafePerformSync

  @Benchmark
  def takeLongs5P: Seq[Long] = Await.result(longStreamP.run(p.Iteratee.takeUpTo(count)), Duration.Inf)

  @Benchmark
  def takeLongs6C: Vector[Long] = longStreamC.take(count).toVector

  @Benchmark
  def takeLongs7F: Vector[Long] = longStreamF.take(count.toLong).runLog.unsafeRun
}
