package io.iteratee.benchmark

import org.scalatest.FlatSpec

class InMemoryBenchmarkSpec extends FlatSpec {
  val benchmark: InMemoryBenchmark = new InMemoryBenchmark
  val sum = 49995000

  "The in-memory benchmark" should "correctly calculate the sum using io.iteratee.modules.id" in {
    assert(benchmark.sumInts0II === sum)
  }

  it should "correctly calculate the sum using io.iteratee.scalaz" in {
    assert(benchmark.sumInts1IT === sum)
  }

  it should "correctly calculate the sum using io.iteratee.twitter" in {
    assert(benchmark.sumInts2IR === sum)
  }

  it should "correctly calculate the sum using scalaz-stream" in {
    assert(benchmark.sumInts3S === sum)
  }

  it should "correctly calculate the sum using scalaz-iteratee" in {
    assert(benchmark.sumInts4Z === sum)
  }

  it should "correctly calculate the sum using play-iteratee" in {
    assert(benchmark.sumInts5P === sum)
  }

  it should "correctly calculate the sum using the collections library" in {
    assert(benchmark.sumInts6C === sum)
  }

  it should "correctly calculate the sum using fs2" in {
    assert(benchmark.sumInts7F === sum)
  }
}
