package io.iteratee.benchmark

import org.scalatest.FlatSpec

class InMemoryBenchmarkSpec extends FlatSpec {
  val benchmark: InMemoryBenchmark = new InMemoryBenchmark
  val sum = 49995000

  "The in-memory benchmark" should "correctly calculate the sum using iteratee.io.task" in {
    assert(benchmark.sumInts0I === sum)
  }

  it should "correctly calculate the sum using iteratee.io.twitter" in {
    assert(benchmark.sumInts1R === sum)
  }

  it should "correctly calculate the sum using scalaz-stream" in {
    assert(benchmark.sumInts2S === sum)
  }

  it should "correctly calculate the sum using scalaz-iteratee" in {
    assert(benchmark.sumInts3Z === sum)
  }

  it should "correctly calculate the sum using play-iteratee" in {
    assert(benchmark.sumInts4P === sum)
  }

  it should "correctly calculate the sum using the collections library" in {
    assert(benchmark.sumInts5C === sum)
  }

  it should "correctly calculate the sum using fs2" in {
    assert(benchmark.sumInts6F === sum)
  }
}
