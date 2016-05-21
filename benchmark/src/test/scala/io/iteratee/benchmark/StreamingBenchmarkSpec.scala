package io.iteratee.benchmark

import org.scalatest.FlatSpec

class StreamingBenchmarkSpec extends FlatSpec {
  val benchmark: StreamingBenchmark = new StreamingBenchmark
  val taken = (0 until 10000).toVector

  "The in-memory benchmark" should "correctly gather elements using io.iteratee.task" in {
    assert(benchmark.takeLongs0I === taken)
  }

  it should "correctly gather elements using io.iteratee.twitter" in {
    assert(benchmark.takeLongs1R === taken)
  }

  it should "correctly gather elements using io.iteratee.monix" in {
    assert(benchmark.takeLongs2M === taken)
  }

  it should "correctly gather elements using scalaz-stream" in {
    assert(benchmark.takeLongs3S === taken)
  }

  it should "correctly gather elements using scalaz-iteratee" in {
    assert(benchmark.takeLongs4Z === taken)
  }

  it should "correctly gather elements using play-iteratee" in {
    assert(benchmark.takeLongs5P === taken)
  }

  it should "correctly gather elements using the collections library" in {
    assert(benchmark.takeLongs6C === taken)
  }

  it should "correctly gather elements using fs2" in {
    assert(benchmark.takeLongs7F === taken)
  }
}
