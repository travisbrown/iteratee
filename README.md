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
summing a sequence of numbers with this library and Scalaz's `Task` (`IS`), this library and Twitter
futures (`IR`), this library and [Monix][monix]'s `Task` ('IM'), Scalaz Stream (`S`),
scalaz-iteratee (`Z`), [play-iteratee][play-iteratee] (`P`), the collections library (`C`), and fs2
(`F`). Higher numbers are better.

```
Benchmark                      Mode  Cnt      Score     Error  Units
InMemoryBenchmark.sumInts0IS  thrpt   80  17371.029 ± 112.907  ops/s
InMemoryBenchmark.sumInts1IR  thrpt   80  11989.788 ± 687.895  ops/s
InMemoryBenchmark.sumInts2IM  thrpt   80  13961.907 ±  51.421  ops/s
InMemoryBenchmark.sumInts3S   thrpt   80     73.853 ±   1.133  ops/s
InMemoryBenchmark.sumInts4Z   thrpt   80    316.888 ±   4.563  ops/s
InMemoryBenchmark.sumInts5P   thrpt   80     49.164 ±   0.857  ops/s
InMemoryBenchmark.sumInts6C   thrpt   80  13061.763 ±  84.603  ops/s
InMemoryBenchmark.sumInts7F   thrpt   80  14870.957 ±  34.222  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                        Mode   Cnt     Score    Error  Units
StreamingBenchmark.takeLongs0IS  thrpt   80  1117.809 ± 20.277  ops/s
StreamingBenchmark.takeLongs1IR  thrpt   80   879.107 ±  2.895  ops/s
StreamingBenchmark.takeLongs2IM  thrpt   80  1932.755 ± 16.407  ops/s
StreamingBenchmark.takeLongs3S   thrpt   80    59.548 ±  0.595  ops/s
StreamingBenchmark.takeLongs4Z   thrpt   80   199.502 ±  0.849  ops/s
StreamingBenchmark.takeLongs5P   thrpt   80     1.714 ±  0.020  ops/s
StreamingBenchmark.takeLongs6C   thrpt   80  3254.250 ± 59.598  ops/s
StreamingBenchmark.takeLongs7F   thrpt   80    23.824 ±  0.101  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                            Mode  Cnt           Score          Error  Units
InMemoryBenchmark.sumInts0IS:gc.alloc.rate.norm     thrpt   10      161701.085 ±       10.485   B/op
InMemoryBenchmark.sumInts1IR:gc.alloc.rate.norm     thrpt   10      161488.050 ±       36.071   B/op
InMemoryBenchmark.sumInts2IM:gc.alloc.rate.norm     thrpt   10      161201.727 ±       12.319   B/op
InMemoryBenchmark.sumInts3S:gc.alloc.rate.norm      thrpt   10    63576633.462 ±      175.370   B/op
InMemoryBenchmark.sumInts4Z:gc.alloc.rate.norm      thrpt   10    16401505.515 ±       12.759   B/op
InMemoryBenchmark.sumInts5P:gc.alloc.rate.norm      thrpt   10    13655654.023 ±    25784.025   B/op
InMemoryBenchmark.sumInts6C:gc.alloc.rate.norm      thrpt   10      159851.915 ±       25.612   B/op
InMemoryBenchmark.sumInts7F:gc.alloc.rate.norm      thrpt   10      195277.788 ±       48.127   B/op


Benchmark                                            Mode  Cnt           Score          Error  Units
StreamingBenchmark.takeLongs0IS:gc.alloc.rate.norm  thrpt   10     5924304.426 ±        0.052   B/op
StreamingBenchmark.takeLongs1IR:gc.alloc.rate.norm  thrpt   10     5524155.660 ±        9.720   B/op
StreamingBenchmark.takeLongs2IM:gc.alloc.rate.norm  thrpt   10     3524904.242 ±        0.009   B/op
StreamingBenchmark.takeLongs3S:gc.alloc.rate.norm   thrpt   10    75207123.724 ±  1019922.216   B/op
StreamingBenchmark.takeLongs4Z:gc.alloc.rate.norm   thrpt   10    28648043.020 ±   382518.213   B/op
StreamingBenchmark.takeLongs5P:gc.alloc.rate.norm   thrpt   10  1206116460.000 ±      494.872   B/op
StreamingBenchmark.takeLongs6C:gc.alloc.rate.norm   thrpt   10      526752.153 ±        0.002   B/op
StreamingBenchmark.takeLongs7F:gc.alloc.rate.norm   thrpt   10   194973075.393 ±        2.065   B/op
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
