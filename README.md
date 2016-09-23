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
summing a sequence of numbers with this library and `cats.Id` (`II`), this library and Scalaz's
`Task` (`IT`), this library and Twitter futures (`IR`), Scalaz Stream (`S`), scalaz-iteratee (`Z`),
[play-iteratee][play-iteratee] (`P`), the collections library (`C`), fs2 (`F`), and Monix Observable (`M`). Higher numbers
are better.

```
Benchmark                      Mode  Cnt      Score     Error  Units
InMemoryBenchmark.sumInts0II  thrpt   80  12891.838 ±  95.943  ops/s
InMemoryBenchmark.sumInts1IT  thrpt   80  11934.148 ± 117.666  ops/s
InMemoryBenchmark.sumInts2IR  thrpt   80  19735.557 ± 161.410  ops/s
InMemoryBenchmark.sumInts3S   thrpt   80     73.350 ±   1.065  ops/s
InMemoryBenchmark.sumInts4Z   thrpt   80    293.150 ±   2.559  ops/s
InMemoryBenchmark.sumInts5P   thrpt   80     44.486 ±   0.899  ops/s
InMemoryBenchmark.sumInts6C   thrpt   80  12816.016 ±  78.990  ops/s
InMemoryBenchmark.sumInts7F   thrpt   80  10276.384 ±  67.759  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                         Mode  Cnt     Score    Error  Units
StreamingBenchmark.takeLongs0IS  thrpt   80  1090.926 ± 21.696  ops/s
StreamingBenchmark.takeLongs1IR  thrpt   80   932.749 ±  7.862  ops/s
StreamingBenchmark.takeLongs2IM  thrpt   80  1485.798 ± 17.697  ops/s
StreamingBenchmark.takeLongs3S   thrpt   80    59.347 ±  0.340  ops/s
StreamingBenchmark.takeLongs4Z   thrpt   80   194.763 ±  3.603  ops/s
StreamingBenchmark.takeLongs5P   thrpt   80     1.313 ±  0.035  ops/s
StreamingBenchmark.takeLongs6C   thrpt   80  3196.885 ± 25.715  ops/s
StreamingBenchmark.takeLongs7F   thrpt   80     6.760 ±  0.036  ops/s

Benchmark                         Mode  Cnt     Score     Error  Units
StreamingBenchmark.takeLongs0II  thrpt   80  2300.547 ±  35.063  ops/s
StreamingBenchmark.takeLongs1IT  thrpt   80  1054.962 ±   7.078  ops/s
StreamingBenchmark.takeLongs2IR  thrpt   80   814.381 ±  19.351  ops/s
StreamingBenchmark.takeLongs3S   thrpt   80    58.351 ±   0.528  ops/s
StreamingBenchmark.takeLongs4Z   thrpt   80   191.689 ±   1.277  ops/s
StreamingBenchmark.takeLongs5P   thrpt   80     1.855 ±   0.081  ops/s
StreamingBenchmark.takeLongs6C   thrpt   80  2780.370 ± 150.970  ops/s
StreamingBenchmark.takeLongs7F   thrpt   80     6.667 ±   0.056  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                            Mode  Cnt          Score          Error  Units
InMemoryBenchmark.sumInts0II:gc.alloc.rate.norm     thrpt   10     161083.473 ±       10.157   B/op
InMemoryBenchmark.sumInts1IT:gc.alloc.rate.norm     thrpt   10     161756.601 ±       10.011   B/op
InMemoryBenchmark.sumInts2IR:gc.alloc.rate.norm     thrpt   10     161554.880 ±        9.051   B/op
InMemoryBenchmark.sumInts3S:gc.alloc.rate.norm      thrpt   10   63416636.376 ±   255002.884   B/op
InMemoryBenchmark.sumInts4Z:gc.alloc.rate.norm      thrpt   10   16401515.857 ±   255007.657   B/op
InMemoryBenchmark.sumInts5P:gc.alloc.rate.norm      thrpt   10   13635867.405 ±    10698.402   B/op
InMemoryBenchmark.sumInts6C:gc.alloc.rate.norm      thrpt   10     159851.919 ±       25.649   B/op
InMemoryBenchmark.sumInts7F:gc.alloc.rate.norm      thrpt   10     288158.919 ±     3021.725   B/op

Benchmark                                            Mode  Cnt          Score          Error  Units
StreamingBenchmark.takeLongs0II:gc.alloc.rate.norm  thrpt   10     3123720.463 ±       0.004   B/op
StreamingBenchmark.takeLongs1IT:gc.alloc.rate.norm  thrpt   10     5924305.297 ±       1.184   B/op
StreamingBenchmark.takeLongs2IR:gc.alloc.rate.norm  thrpt   10     5204125.702 ±       9.920   B/op
StreamingBenchmark.takeLongs3S:gc.alloc.rate.norm   thrpt   10    75607211.366 ±  255032.949   B/op
StreamingBenchmark.takeLongs4Z:gc.alloc.rate.norm   thrpt   10    28888073.078 ±      17.792   B/op
StreamingBenchmark.takeLongs5P:gc.alloc.rate.norm   thrpt   10  1206276610.133 ±    1008.603   B/op
StreamingBenchmark.takeLongs6C:gc.alloc.rate.norm   thrpt   10      526752.381 ±       0.005   B/op
StreamingBenchmark.takeLongs7F:gc.alloc.rate.norm   thrpt   10   650663709.714 ±  127478.253   B/op
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
