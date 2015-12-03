# iteratee

[![Join the chat at https://gitter.im/travisbrown/iteratee](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/travisbrown/iteratee?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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
look promising:

```
Benchmark                      Mode  Cnt     Score    Error  Units
IterateeBenchmark.sumIntsI    thrpt   20  2161.012 ± 76.171  ops/s
IterateeBenchmark.sumIntsS    thrpt   20   529.520 ± 15.892  ops/s
IterateeBenchmark.takeLongsI  thrpt   20   953.304 ±  7.715  ops/s
IterateeBenchmark.takeLongsS  thrpt   20   369.583 ±  2.474  ops/s
```

```
IterateeBenchmark.sumIntsI:gc.alloc.rate.norm   thrpt   20   1760856.770 ±       1.491    B/op
IterateeBenchmark.sumIntsS:gc.alloc.rate.norm   thrpt   20   8001675.187 ±      34.398    B/op
IterateeBenchmark.takeLongsI:gc.alloc.rate.norm thrpt   20   4724417.873 ±       3.656    B/op
IterateeBenchmark.takeLongsS:gc.alloc.rate.norm thrpt   20  11685721.549 ±  356398.849    B/op
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
