package io.iteratee.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import scala.Predef.intWrapper

class StreamingBenchmarkSpec extends AnyFlatSpec {
  val benchmark: StreamingBenchmark = new StreamingBenchmark
  val taken = (0 until 10000).toVector

  "The streaming benchmark" should "correctly gather elements using io.iteratee.modules.id" in {
    assert(benchmark.takeLongs0II === taken)
  }

  it should "correctly gather elements using cats.effect.IO" in {
    assert(benchmark.takeLongs1IO === taken)
  }

  it should "correctly gather elements using fs2" in {
    assert(benchmark.takeLongs3F === taken)
  }

  it should "correctly gather elements using the collections library" in {
    assert(benchmark.takeLongs4C === taken)
  }
}
