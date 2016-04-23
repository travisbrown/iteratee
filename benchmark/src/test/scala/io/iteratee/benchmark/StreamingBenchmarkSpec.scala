package io.iteratee.benchmark

import org.scalatest.FlatSpec

class StreamingBenchmarkSpec extends FlatSpec {
  val benchmark: StreamingBenchmark = new StreamingBenchmark
  val taken = (0 until 10000).toVector

  "The in-memory benchmark" should "correctly gather elements using iteratee.io.task" in {
    assert(benchmark.takeLongs0I === taken)
  }

  it should "correctly gather elements using iteratee.io.twitter" in {
    assert(benchmark.takeLongs1R === taken)
  }

  it should "correctly gather elements using scalaz-stream" in {
    assert(benchmark.takeLongs2S === taken)
  }

  it should "correctly gather elements using scalaz-iteratee" in {
    assert(benchmark.takeLongs3Z === taken)
  }

  it should "correctly gather elements using play-iteratee" in {
    assert(benchmark.takeLongs4P === taken)
  }

  it should "correctly gather elements using the collections library" in {
    assert(benchmark.takeLongs5C === taken)
  }

  it should "correctly gather elements using fs2" in {
    assert(benchmark.takeLongs6F === taken)
  }
}
