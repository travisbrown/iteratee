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
[play-iteratee][play-iteratee] (`P`), and the collections library (`C`). Higher numbers are better.

```
Benchmark                       Mode  Cnt      Score    Error  Units
InMemoryBenchmark.sumInts0I     thrpt   80  15105.537 ± 25.871  ops/s
InMemoryBenchmark.sumInts1S     thrpt   80     78.947 ±  0.510  ops/s
InMemoryBenchmark.sumInts2Z     thrpt   80    296.223 ±  1.971  ops/s
InMemoryBenchmark.sumInts3P     thrpt   80     57.355 ±  0.745  ops/s
InMemoryBenchmark.sumInts4C     thrpt   80  13056.163 ± 22.790  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                       Mode  Cnt      Score    Error  Units
StreamingBenchmark.takeLongs0I  thrpt   80   1146.021 ±  6.539  ops/s
StreamingBenchmark.takeLongs1S  thrpt   80     65.916 ±  0.182  ops/s
StreamingBenchmark.takeLongs2Z  thrpt   80    198.919 ±  2.097  ops/s
StreamingBenchmark.takeLongs3P  thrpt   80      1.447 ±  0.082  ops/s
StreamingBenchmark.takeLongs4C  thrpt   80   3286.878 ± 37.967  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                           Mode  Cnt             Score         Error   Units
InMemoryBenchmark.sumInts0I:gc.alloc.rate.norm     thrpt   10      161688.037 ±        11.415    B/op
InMemoryBenchmark.sumInts1S:gc.alloc.rate.norm     thrpt   10    58493412.522 ±   1083990.715    B/op
InMemoryBenchmark.sumInts2Z:gc.alloc.rate.norm     thrpt   10    16881441.584 ±         0.044    B/op
InMemoryBenchmark.sumInts3P:gc.alloc.rate.norm     thrpt   10    13468439.557 ±    267296.027    B/op
InMemoryBenchmark.sumInts4C:gc.alloc.rate.norm     thrpt   10      159846.149 ±        29.299    B/op

Benchmark                                           Mode  Cnt             Score         Error   Units
StreamingBenchmark.takeLongs0I:gc.alloc.rate.norm  thrpt   10     5924322.065 ±        31.187    B/op
StreamingBenchmark.takeLongs1S:gc.alloc.rate.norm  thrpt   10    70326819.219 ±    637512.439    B/op
StreamingBenchmark.takeLongs2Z:gc.alloc.rate.norm  thrpt   10    28647967.493 ±    254993.412    B/op
StreamingBenchmark.takeLongs3P:gc.alloc.rate.norm  thrpt   10  1206114735.600 ±      7941.695    B/op
StreamingBenchmark.takeLongs4C:gc.alloc.rate.norm  thrpt   10      526752.174 ±         0.033    B/op
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
