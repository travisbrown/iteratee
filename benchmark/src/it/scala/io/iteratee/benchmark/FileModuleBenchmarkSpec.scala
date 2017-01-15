package io.iteratee.benchmark

import org.scalatest.FlatSpec

class FileModuleBenchmarkSpec extends FlatSpec {
  val benchmark: FileModuleBenchmark = new FileModuleBenchmark
  val length = 4.516696895337672

  "The FileModule benchmark" should "correctly calculate the average word length using Rerunnable" in {
    assert(benchmark.avgWordLengthTR === length)
  }

  it should "correctly calculate the average word length using Twitter's Future" in {
    assert(benchmark.avgWordLengthTF === length)
  }

  it should "correctly calculate the average word length using Twitter's Try" in {
    assert(benchmark.avgWordLengthTT === length)
  }

  it should "correctly calculate the average word length using Twitter's Try in Free" in {
    assert(benchmark.avgWordLengthTTF === length)
  }

  it should "correctly calculate the average word length using Scalaz's Task" in {
    assert(benchmark.avgWordLengthS === length)
  }

  it should "correctly calculate the average word length using Monix's Task" in {
    assert(benchmark.avgWordLengthM === length)
  }

  it should "correctly calculate the average word length using FS2's Task" in {
    assert(benchmark.avgWordLengthF === length)
  }
}
