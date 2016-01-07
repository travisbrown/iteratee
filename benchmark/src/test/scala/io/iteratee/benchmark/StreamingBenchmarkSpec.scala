package io.iteratee.benchmark

import org.scalatest.FlatSpec

class StreamingBenchmarkSpec extends FlatSpec {
  val benchmark: StreamingBenchmark = new StreamingBenchmark
  val taken = (0 until 10000).toVector

  "The in-memory benchmark" should "correctly calculate the sum using iteratee.io" in {
    assert(benchmark.takeLongs0I === taken)
  }

  it should "correctly calculate the sum using scalaz-stream" in {
    assert(benchmark.takeLongs1S === taken)
  }

  it should "correctly calculate the sum using scalaz-iteratee" in {
    assert(benchmark.takeLongs2Z === taken)
  }

  it should "correctly calculate the sum using play-iteratee" in {
    assert(benchmark.takeLongs3P === taken)
  }

  it should "correctly calculate the sum using the collections library" in {
    assert(benchmark.takeLongs4C === taken)
  }
}
