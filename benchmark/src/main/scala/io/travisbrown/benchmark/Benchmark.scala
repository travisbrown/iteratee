package io.travisbrown.benchmark

import cats.std.all._
import io.travisbrown.{ iteratee => i }
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.{ iteratee => s }
import scalaz.Scalaz._

class ExampleData {
  val maxSize = 200
  val intsI: i.Enumerator[Int] = i.EnumeratorT.enumStream((0 to maxSize).toStream)
  val intsS: s.Enumerator[Int] = s.EnumeratorT.enumStream((0 to maxSize).toStream)
}

/**
 * Compare the performance of iteratee operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "benchmark/run -i 10 -wi 10 -f 2 -t 1 io.travisbrown.benchmark.IterateeBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IterateeBenchmark extends ExampleData {
  @Benchmark
  def sumIntsI: Int = (i.IterateeT.sum[Int, cats.Id] &= intsI).run

  @Benchmark
  def sumIntsS: Int = (s.IterateeT.sum[Int, scalaz.Id.Id] &= intsS).run
}
