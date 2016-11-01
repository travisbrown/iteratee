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
summing a sequence of numbers with this library and `cats.Id` (`II`), this library and Monix's
`Task` (`IM`), this library and Scalaz's `Task` (`IT`), this library and Twitter futures (`IR`),
Scalaz Stream (`S`), scalaz-iteratee (`Z`), [play-iteratee][play-iteratee] (`P`), the Scala
collections library (`C`), and fs2 (`F`). Higher numbers are better.

```
Benchmark                      Mode  Cnt      Score     Error  Units
InMemoryBenchmark.sumInts0II  thrpt   80  10225.388 ± 191.612  ops/s
InMemoryBenchmark.sumInts1IM  thrpt   80  13395.800 ±  30.912  ops/s
InMemoryBenchmark.sumInts2IT  thrpt   80  18609.579 ±  47.491  ops/s
InMemoryBenchmark.sumInts3IR  thrpt   80  15999.740 ± 114.949  ops/s
InMemoryBenchmark.sumInts4S   thrpt   80     72.074 ±   1.209  ops/s
InMemoryBenchmark.sumInts5Z   thrpt   80    310.472 ±   4.368  ops/s
InMemoryBenchmark.sumInts6P   thrpt   80     43.071 ±   0.543  ops/s
InMemoryBenchmark.sumInts7C   thrpt   80  12975.042 ±  48.702  ops/s
InMemoryBenchmark.sumInts8F   thrpt   80   9610.699 ±  41.936  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                         Mode  Cnt     Score    Error  Units
StreamingBenchmark.takeLongs0II  thrpt   80  2787.725 ± 16.812  ops/s
StreamingBenchmark.takeLongs1IM  thrpt   80  1617.848 ± 19.899  ops/s
StreamingBenchmark.takeLongs2IT  thrpt   80  1052.494 ±  7.707  ops/s
StreamingBenchmark.takeLongs3IR  thrpt   80   979.514 ± 26.197  ops/s
StreamingBenchmark.takeLongs4S   thrpt   80    56.882 ±  0.969  ops/s
StreamingBenchmark.takeLongs5Z   thrpt   80   154.103 ± 10.350  ops/s
StreamingBenchmark.takeLongs6P   thrpt   80     1.216 ±  0.005  ops/s
StreamingBenchmark.takeLongs7C   thrpt   80  3273.158 ± 55.187  ops/s
StreamingBenchmark.takeLongs8F   thrpt   80     7.915 ±  0.044  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                            Mode  Cnt           Score          Error  Units
InMemoryBenchmark.sumInts0II:gc.alloc.rate.norm     thrpt   20      159953.462 ±       11.863   B/op
InMemoryBenchmark.sumInts1IM:gc.alloc.rate.norm     thrpt   20      160203.272 ±        5.949   B/op
InMemoryBenchmark.sumInts2IT:gc.alloc.rate.norm     thrpt   20      160622.026 ±        6.323   B/op
InMemoryBenchmark.sumInts3IR:gc.alloc.rate.norm     thrpt   20      160398.303 ±        6.685   B/op
InMemoryBenchmark.sumInts4S:gc.alloc.rate.norm      thrpt   20    63936897.241 ±   320928.043   B/op
InMemoryBenchmark.sumInts5Z:gc.alloc.rate.norm      thrpt   20    16401510.998 ±        6.115   B/op
InMemoryBenchmark.sumInts6P:gc.alloc.rate.norm      thrpt   20    13802446.593 ±   229152.745   B/op
InMemoryBenchmark.sumInts7C:gc.alloc.rate.norm      thrpt   20      159851.547 ±       14.556   B/op
InMemoryBenchmark.sumInts8F:gc.alloc.rate.norm      thrpt   20      260454.260 ±     1522.736   B/op

Benchmark                                            Mode  Cnt           Score          Error  Units
StreamingBenchmark.takeLongs0II:gc.alloc.rate.norm  thrpt   20     3043720.338 ±        0.018   B/op
StreamingBenchmark.takeLongs1IM:gc.alloc.rate.norm  thrpt   20     3444961.639 ±        4.168   B/op
StreamingBenchmark.takeLongs2IT:gc.alloc.rate.norm  thrpt   20     5804308.795 ±    61718.228   B/op
StreamingBenchmark.takeLongs3IR:gc.alloc.rate.norm  thrpt   20     5124124.296 ±        5.147   B/op
StreamingBenchmark.takeLongs4S:gc.alloc.rate.norm   thrpt   20    75347149.315 ±   555268.150   B/op
StreamingBenchmark.takeLongs5Z:gc.alloc.rate.norm   thrpt   20    28588033.048 ±   238419.245   B/op
StreamingBenchmark.takeLongs6P:gc.alloc.rate.norm   thrpt   20  1206196498.000 ±    71329.621   B/op
StreamingBenchmark.takeLongs7C:gc.alloc.rate.norm   thrpt   20      526752.310 ±        0.029   B/op
StreamingBenchmark.takeLongs8F:gc.alloc.rate.norm   thrpt   20   531380973.839 ± 13505581.754   B/op
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
[monix]: https://github.com/monixio/monix
[play-iteratee]: https://www.playframework.com/documentation/2.5.x/Iteratees
[scalaz]: https://github.com/scalaz/scalaz
[scalaz-iteratee]: https://github.com/scalaz/scalaz/tree/series/7.2.x/iteratee/src/main/scala/scalaz/iteratee
