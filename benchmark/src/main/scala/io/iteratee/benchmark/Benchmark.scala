package io.iteratee.benchmark

import cats.std.int._
import cats.std.list.{ listAlgebra, listInstance => listInstanceC }
import io.{ iteratee => i }
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.{ iteratee => s }
import scalaz.std.anyVal.intInstance
import scalaz.std.list.{ listInstance => listInstanceS, listMonoid }
import scalaz.std.vector._

class ExampleData {
  val maxSize = 10000
  val intsI: i.Enumerator[Int, cats.Eval] = i.Enumerator.enumVector((0 to maxSize).toVector)
  val intsS: s.EnumeratorT[Int, scalaz.Free.Trampoline] =
    s.EnumeratorT.enumList((0 to maxSize).toList)

  val longsI: i.Enumerator[Long, cats.Eval] = i.Enumerator.iterate[Long, cats.Eval](0L)(_ + 1L)
  val longsS: s.EnumeratorT[Long, scalaz.Free.Trampoline] =
    s.EnumeratorT.iterate[Long, scalaz.Free.Trampoline](_ + 1L, 0L)
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
  def sumIntsI: Int = i.Iteratee.sum[Int, cats.Eval].process(intsI).value

  @Benchmark
  def sumIntsS: Int = (s.IterateeT.sum[Int, scalaz.Free.Trampoline] &= intsS).run.run

  @Benchmark
  def takeLongsI: Vector[Long] = i.Iteratee.take[Long, cats.Eval](10000).process(longsI).value

  @Benchmark
  def takeLongsS: Vector[Long] =
    (s.Iteratee.take[Long, Vector](10000).up[scalaz.Free.Trampoline] &= longsS).run.run
}
