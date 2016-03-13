import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization := "io.iteratee",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8")
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture"
)

lazy val catsVersion = "0.4.1"
lazy val disciplineVersion = "0.4"
lazy val scalaCheckVersion = "1.12.5"
lazy val scalaTestVersion = "3.0.0-M9"

lazy val baseSettings = Seq(
  scalacOptions ++= compilerOptions ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => Seq("-Ywarn-unused-import")
      case _ => Nil
    }
  ),
  scalacOptions in (Compile, console) := compilerOptions,
  scalacOptions in (Compile, test) := compilerOptions ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => Seq("-Ywarn-unused-import")
      case _ => Nil
    }
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  coverageHighlighting := (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => false
      case _ => true
    }
  ),
  (scalastyleSources in Compile) <++= sourceDirectories in Compile
)
lazy val allSettings = buildSettings ++ baseSettings ++ publishSettings

lazy val commonJsSettings = Seq(
  postLinkJSEnv := NodeJSEnv().value,
  scalaJSStage in Global := FastOptStage
)

lazy val docSettings = site.settings ++ ghpages.settings ++ unidocSettings ++ Seq(
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-groups",
    "-implicits",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:travisbrown/iteratee.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inAnyProject -- inProjects(coreJS, benchmark, tests, testsJS)
)

lazy val root = project.in(file("."))
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(core, coreJS, task, tests, testsJS, benchmark)
  .dependsOn(core)

lazy val coreBase = crossProject.crossType(CrossType.Pure).in(file("core"))
  .settings(
    moduleName := "iteratee-core",
    name := "core"
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "core"))
  .jsConfigure(_.copy(id = "coreJS"))

lazy val core = coreBase.jvm
lazy val coreJS = coreBase.js

lazy val testsBase = crossProject.in(file("tests"))
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-tests",
    name := "tests"
  )
  .settings(allSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion,
      "org.scalatest" %% "scalatest" % scalaTestVersion,
      "org.typelevel" %% "cats-laws" % catsVersion,
      "org.typelevel" %% "discipline" % disciplineVersion
    ),
    parallelExecution in Test := true,
    testForkedParallel in Test := true,
    parallelExecution in IntegrationTest := true,
    testForkedParallel in IntegrationTest := true
  )
  .settings(
    coverageExcludedPackages := "io\\.iteratee\\.tests\\..*",
    testOptions in Test ++= (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => Seq(Tests.Argument("-l", "io.iteratee.tests.NoScala210Test"))
        case _ => Nil
      }
    )
  )
  .jvmSettings(fork := false)
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "tests").dependsOn(task))
  .jsConfigure(_.copy(id = "testsJS"))
  .dependsOn(coreBase)

lazy val tests = testsBase.jvm
lazy val testsJS = testsBase.js

lazy val task = project
  .settings(
    moduleName := "iteratee-task"
  )
  .settings(allSettings)
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.7"
  ).dependsOn(core)

lazy val benchmark = project
  .settings(moduleName := "iteratee-benchmark")
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "0.9.0-SNAPSHOT",
      "com.typesafe.play" %% "play-iteratees" % "2.5.0",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.scalaz" %% "scalaz-iteratee" % "7.1.7",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8"
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core, task)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/travisbrown/iteratee")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  autoAPIMappings := true,
  apiURL := Some(url("https://travisbrown.github.io/iteratee/api/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/travisbrown/iteratee"),
      "scm:git:git@github.com:travisbrown/iteratee.git"
    )
  ),
  pomExtra := (
    <developers>
      <developer>
        <id>travisbrown</id>
        <name>Travis Brown</name>
        <url>https://twitter.com/travisbrown</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials ++= (
  for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username,
    password
  )
).toSeq

val jvmProjects = Seq(
  "core",
  "task",
  "tests"
)

val jsProjects = Seq(
  "coreJS",
  "testsJS"
)

addCommandAlias("buildJVM", jvmProjects.map(";" + _ + "/compile").mkString)
addCommandAlias("validateJVM", ";buildJVM;tests/test;tests/it:test;benchmark/test;scalastyle;unidoc")
addCommandAlias("buildJS", jsProjects.map(";" + _ + "/compile").mkString)
addCommandAlias("validateJS", ";buildJS;testsJS/test;testsJS/it:test;scalastyle")
addCommandAlias("validate", ";validateJVM;validateJS")
