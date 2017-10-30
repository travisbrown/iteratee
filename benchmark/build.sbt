organization in ThisBuild := "io.iteratee"

val catsVersion = "1.0.0-MF"
val iterateeVersion = "0.14.0"
val scalaTestVersion = "3.0.4"

val benchmark = project.in(file("."))
  .configs(IntegrationTest)
  .enablePlugins(JmhPlugin)
  .settings(Defaults.itSettings)
  .settings(
    crossScalaVersions := Seq("2.11.11", "2.12.4"),
    moduleName := "iteratee-benchmark",
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary),
    libraryDependencies ++= Seq(
      "io.iteratee" %% "iteratee-fs2" % iterateeVersion,
      "io.iteratee" %% "iteratee-monix" % iterateeVersion,
      "io.iteratee" %% "iteratee-scalaz" % iterateeVersion,
      "io.iteratee" %% "iteratee-testing" % iterateeVersion % "test,it",
      "io.iteratee" %% "iteratee-twitter" % iterateeVersion,
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test,it",
      "org.scalaz" %% "scalaz-iteratee" % "7.2.16",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a",
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )
