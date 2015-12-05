# iteratee

[![Build status](https://img.shields.io/travis/travisbrown/iteratee/master.svg)](https://travis-ci.org/travisbrown/iteratee)
[![Coverage status](https://img.shields.io/codecov/c/github/travisbrown/iteratee/master.svg)](https://codecov.io/github/travisbrown/iteratee)
[![Gitter](https://img.shields.io/badge/gitter-join%20chat-green.svg)](https://gitter.im/travisbrown/iteratee)
[![Maven Central](https://img.shields.io/maven-central/v/io.iteratee/iteratee-core_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/io.iteratee/iteratee-core_2.11)

This is a quick port of [Scalaz][scalaz]'s [iteratee implementation][scalaz-iteratee] to
[cats][cats].

The motivations for the port are similar to those for [circe][circe]—in particular I'm aiming for a
more consistent API, better performance, and better documentation.

So far I've made two major optimizations: I've added chunking for input and `InputFolder` and
`StepFolder` classes that combine "functions" for folds into a single object. The initial results
look promising. For example, here are the throughput results for summing a sequence a numbers for
the collections library (`C`), this library (`I`), scalaz-stream (`S`), and Scalaz iteratees (`Z`).
Higher numbers are better.

```
Benchmark                    Mode  Cnt      Score    Error  Units
InMemoryBenchmark.sumIntsC  thrpt   40  13020.825 ± 38.522  ops/s
InMemoryBenchmark.sumIntsI  thrpt   40   2783.060 ± 12.597  ops/s
InMemoryBenchmark.sumIntsS  thrpt   40     71.098 ±  0.298  ops/s
InMemoryBenchmark.sumIntsZ  thrpt   40    308.207 ±  2.400  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                       Mode  Cnt     Score     Error  Units
StreamingBenchmark.takeLongsC  thrpt   40  2898.525 ± 208.089  ops/s
StreamingBenchmark.takeLongsI  thrpt   40   826.677 ±  27.975  ops/s
StreamingBenchmark.takeLongsS  thrpt   40    61.036 ±   0.660  ops/s
StreamingBenchmark.takeLongsZ  thrpt   40   333.113 ±   2.792  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                           Mode  Cnt         Score         Error   Units
InMemoryBenchmark.sumIntsC:·gc.alloc.rate.norm     thrpt   20    159864.130 ±       0.251    B/op
InMemoryBenchmark.sumIntsI:·gc.alloc.rate.norm     thrpt   20   1520760.659 ±       1.288    B/op
InMemoryBenchmark.sumIntsS:·gc.alloc.rate.norm     thrpt   20  59373920.222 ±   35829.334    B/op
InMemoryBenchmark.sumIntsZ:·gc.alloc.rate.norm     thrpt   20  14081129.846 ±      13.693    B/op

Benchmark                                           Mode  Cnt         Score         Error   Units
StreamingBenchmark.takeLongsC:·gc.alloc.rate.norm  thrpt   20    526752.652 ±       1.256    B/op
StreamingBenchmark.takeLongsI:·gc.alloc.rate.norm  thrpt   20   5284138.025 ±       3.949    B/op
StreamingBenchmark.takeLongsS:·gc.alloc.rate.norm  thrpt   20  69766827.562 ±      53.377    B/op
StreamingBenchmark.takeLongsZ:·gc.alloc.rate.norm  thrpt   20  13405965.216 ±   35697.982    B/op
```

## License

iteratee is licensed under the **[Apache License, Version 2.0][apache]** (the
"License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[apache]: http://www.apache.org/licenses/LICENSE-2.0
[cats]: https://github.com/non/cats
[circe]: https://github.com/travisbrown/circe
[scalaz]: https://github.com/scalaz/scalaz
[scalaz-iteratee]: https://github.com/scalaz/scalaz/tree/series/7.2.x/iteratee/src/main/scala/scalaz/iteratee
