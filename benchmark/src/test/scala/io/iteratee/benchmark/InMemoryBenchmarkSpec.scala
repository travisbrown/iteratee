package io.iteratee.benchmark

import org.scalatest.FlatSpec

class InMemoryBenchmarkSpec extends FlatSpec {
  val benchmark: InMemoryBenchmark = new InMemoryBenchmark
  val sum = 49995000

  "The in-memory benchmark" should "correctly calculate the sum using io.iteratee.modules.id" in {
    assert(benchmark.sumInts0II === sum)
  }

  it should "correctly calculate the sum using io.iteratee.monix" in {
    assert(benchmark.sumInts1IM === sum)
  }

  it should "correctly calculate the sum using io.iteratee.scalaz" in {
    assert(benchmark.sumInts2IT === sum)
  }

  it should "correctly calculate the sum using io.iteratee.twitter" in {
    assert(benchmark.sumInts3IR === sum)
  }

  it should "correctly calculate the sum using scalaz-stream" in {
    assert(benchmark.sumInts4S === sum)
  }

  it should "correctly calculate the sum using scalaz-iteratee" in {
    assert(benchmark.sumInts5Z === sum)
  }

  it should "correctly calculate the sum using fs2" in {
    assert(benchmark.sumInts6F === sum)
  }

  it should "correctly calculate the sum using the collections library" in {
    assert(benchmark.sumInts7C === sum)
  }
}
