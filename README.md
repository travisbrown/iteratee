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
Benchmark                    Mode  Cnt      Score     Error  Units
InMemoryBenchmark.sumIntsC  thrpt   20  12747.660 ± 472.606  ops/s
InMemoryBenchmark.sumIntsI  thrpt   20   2185.291 ±  30.460  ops/s
InMemoryBenchmark.sumIntsS  thrpt   20     71.360 ±   0.503  ops/s
InMemoryBenchmark.sumIntsZ  thrpt   20    302.672 ±   7.204  ops/s
```

And the results for collecting the first 10,000 values from an infinite stream of non-negative
numbers into a `Vector`:

```
Benchmark                       Mode  Cnt     Score    Error  Units
StreamingBenchmark.takeLongsC  thrpt   20  3138.873 ± 12.785  ops/s
StreamingBenchmark.takeLongsI  thrpt   20   929.396 ± 51.928  ops/s
StreamingBenchmark.takeLongsS  thrpt   20    59.810 ±  0.310  ops/s
StreamingBenchmark.takeLongsZ  thrpt   20   328.380 ±  2.254  ops/s
```

And allocation rates (lower is better):

```
Benchmark                                         Mode Cnt        Score        Error Units
StreamingBenchmark.takeLongsC:gc.alloc.rate.norm thrpt  20   526752.627 ±      1.223  B/op
StreamingBenchmark.takeLongsI:gc.alloc.rate.norm thrpt  20  4724169.862 ±      3.414  B/op
StreamingBenchmark.takeLongsS:gc.alloc.rate.norm thrpt  20 69766838.125 ±     57.503  B/op
StreamingBenchmark.takeLongsZ:gc.alloc.rate.norm thrpt  20 13445946.932 ± 356356.118  B/op
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
