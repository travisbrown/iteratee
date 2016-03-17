# iteratee.io

[![Build status](https://img.shields.io/travis/travisbrown/iteratee/master.svg)](https://travis-ci.org/travisbrown/iteratee)
[![Coverage status](https://img.shields.io/codecov/c/github/travisbrown/iteratee/master.svg)](https://codecov.io/github/travisbrown/iteratee)
[![Gitter](https://img.shields.io/badge/gitter-join%20chat-green.svg)](https://gitter.im/travisbrown/iteratee)
[![Maven Central](https://img.shields.io/maven-central/v/io.iteratee/iteratee-core_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/io.iteratee/iteratee-core_2.11)

This project is an iteratee implementation for [Cats][cats] that began as a port of
[Scalaz][scalaz]'s [iteratee package][scalaz-iteratee], although the API and implementation are now
very different from Scalaz's. There are [API docs][api-docs] (but they're a work in progress), and
I've published a [blog post][intro] introducing the project.

The motivations for the port are similar to those for [circe][circe]—in particular I'm aiming for a
more consistent API, better performance, and better documentation.

Note that this library doesn't support many of the use cases that [fs2][fs2] (formerly Scalaz
Stream) is designed to handle. It doesn't support nondeterministic reading from multiple streams,
for example, and in general is a less appropriate choice for situations where concurrency and
parallelism are primary goals. Where the use cases of fs2 and this library do overlap, however, it's
often likely to be a simpler, faster solution.

The initial performance benchmarks look promising. For example, here are the throughput results for
summing a sequence of numbers with this library (`I`), Scalaz Stream (`S`), scalaz-iteratee (`Z`),
[play-iteratee][play-iteratee] (`P`), the collections library (`C`), and fs2 (`F`). Higher numbers
are better.

```
Benchmark                        Mode  Cnt      Score     Error  Units
InMemoryBenchmark.sumInts0I     thrpt   80  17451.805 ± 144.119  ops/s
InMemoryBenchmark.sumInts1S     thrpt   80     80.756 ±   0.291  ops/s
InMemoryBenchmark.sumInts2Z     thrpt   80    311.448 ±   1.380  ops/s
InMemoryBenchmark.sumInts3P     thrpt   80    106.916 ±   1.745  ops/s
InMemoryBenchmark.sumInts4C     thrpt   80  20780.020 ± 124.742  ops/s
InMemoryBenchmark.sumInts5F     thrpt   80  18916.338 ±  58.642  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                       Mode  Cnt      Score    Error  Units
StreamingBenchmark.takeLongs0I  thrpt   80   1252.230 ±   3.309  ops/s
StreamingBenchmark.takeLongs1S  thrpt   80     64.888 ±   0.229  ops/s
StreamingBenchmark.takeLongs2Z  thrpt   80    227.533 ±   1.394  ops/s
StreamingBenchmark.takeLongs3P  thrpt   80      1.366 ±   0.005  ops/s
StreamingBenchmark.takeLongs4C  thrpt   80   3000.368 ±   4.800  ops/s
StreamingBenchmark.takeLongs5F  thrpt   80    117.318 ±   0.379  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                           Mode  Cnt           Score           Error   Units
InMemoryBenchmark.sumInts0I:gc.alloc.rate.norm     thrpt   10      161656.027 ±         0.001    B/op
InMemoryBenchmark.sumInts1S:gc.alloc.rate.norm     thrpt   10    57413029.853 ±         0.463    B/op
InMemoryBenchmark.sumInts2Z:gc.alloc.rate.norm     thrpt   10    16881441.657 ±         0.499    B/op
InMemoryBenchmark.sumInts3P:gc.alloc.rate.norm     thrpt   10    14007882.247 ±    253736.629    B/op
InMemoryBenchmark.sumInts4C:gc.alloc.rate.norm     thrpt   10      159864.022 ±         0.001    B/op
InMemoryBenchmark.sumInts5F:gc.alloc.rate.norm     thrpt   10      170832.024 ±         0.001    B/op

Benchmark                                           Mode  Cnt           Score           Error   Units
StreamingBenchmark.takeLongs0I:gc.alloc.rate.norm  thrpt   10     5924360.668 ±        46.481    B/op
StreamingBenchmark.takeLongs1S:gc.alloc.rate.norm  thrpt   10    67926671.128 ±         0.052    B/op
StreamingBenchmark.takeLongs2Z:gc.alloc.rate.norm  thrpt   10    27287730.054 ±         0.130    B/op
StreamingBenchmark.takeLongs3P:gc.alloc.rate.norm  thrpt   10  1206116390.400 ±       250.142    B/op
StreamingBenchmark.takeLongs4C:gc.alloc.rate.norm  thrpt   10      526752.154 ±         0.001    B/op
StreamingBenchmark.takeLongs5F:gc.alloc.rate.norm  thrpt   10    47206075.959 ±         0.047    B/op
```

## License

iteratee.io is licensed under the **[Apache License, Version 2.0][apache]** (the
"License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[apache]: http://www.apache.org/licenses/LICENSE-2.0
[api-docs]: http://travisbrown.github.io/iteratee/api/#io.iteratee.package
[cats]: https://github.com/typelevel/cats
[circe]: https://github.com/travisbrown/circe
[fs2]: https://github.com/functional-streams-for-scala/fs2
[intro]: https://meta.plasm.us/posts/2016/01/08/yet-another-iteratee-library/
[play-iteratee]: https://www.playframework.com/documentation/2.5.x/Iteratees
[scalaz]: https://github.com/scalaz/scalaz
[scalaz-iteratee]: https://github.com/scalaz/scalaz/tree/series/7.2.x/iteratee/src/main/scala/scalaz/iteratee
