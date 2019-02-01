import ReleaseTransformations._
import org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings
import sbtcrossproject.{ crossProject, CrossType }
import scala.xml.{ Elem, Node => XmlNode, NodeSeq => XmlNodeSeq }
import scala.xml.transform.{ RewriteRule, RuleTransformer }

organization in ThisBuild := "io.iteratee"

val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Xfuture"
)

val catsVersion = "1.6.0"
val catsEffectVersion = "1.2.0"
val monixVersion = "3.0.0-RC2"
val scalazVersion = "7.2.27"
val scalazStreamVersion = "0.8.6a"
val fs2Version = "1.0.3"

val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.13.5"
val disciplineVersion = "0.9.0"

/**
 * Some terrible hacks to work around Cats's decision to have builds for
 * different Scala versions depend on different versions of Discipline, etc.
 */
def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

def scalaTestVersionFor(scalaVersion: String): String =
  if (priorTo2_13(scalaVersion)) scalaTestVersion else "3.0.6-SNAP5"

def scalaCheckVersionFor(scalaVersion: String): String =
  if (priorTo2_13(scalaVersion)) scalaCheckVersion else "1.14.0"

def disciplineVersionFor(scalaVersion: String): String =
  if (priorTo2_13(scalaVersion)) disciplineVersion else "0.11.0"

lazy val previousIterateeVersion = "0.18.0"

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

lazy val baseSettings = Seq(
  scalacOptions ++= {
    if (priorTo2_13(scalaVersion.value)) compilerOptions
    else
      compilerOptions.flatMap {
        case "-Ywarn-unused-import" => Some("-Ywarn-unused:imports")
        case "-Yno-adapted-args"    => None
        case other                  => Some(other)
      }
  },
  scalacOptions += "-Yno-predef",
  scalacOptions in (Compile, console) := {
    if (priorTo2_13(scalaVersion.value)) compilerOptions
    else
      compilerOptions.flatMap {
        case "-Ywarn-unused-import" => Some("-Ywarn-unused:imports")
        case "-Yno-adapted-args"    => None
        case other                  => Some(other)
      }
  },
  scalacOptions in (Compile, test) := {
    if (priorTo2_13(scalaVersion.value)) compilerOptions
    else
      compilerOptions.flatMap {
        case "-Ywarn-unused-import" => Some("-Ywarn-unused:imports")
        case "-Yno-adapted-args"    => None
        case other                  => Some(other)
      }
  },
  coverageHighlighting := true,
  (scalastyleSources in Compile) ++= (sourceDirectories in Compile).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.9" cross CrossVersion.binary)
)

lazy val allSettings = baseSettings ++ publishSettings

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage
)

lazy val docSettings = Seq(
  docMappingsApiDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docMappingsApiDir),
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-groups",
    "-implicits",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:travisbrown/iteratee.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inAnyProject -- inProjects(coreJS, benchmark, monixJS, testingJS, testsJVM, testsJS)
)

lazy val iteratee = project
  .in(file("."))
  .enablePlugins(GhpagesPlugin, ScalaUnidocPlugin)
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(benchmark, coreJVM, coreJS, files, monixJVM, monixJS, scalaz, testingJVM, testingJS, testsJVM, testsJS)
  .dependsOn(coreJVM, scalaz)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    moduleName := "iteratee-core",
    name := "core"
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
  )
  .jvmSettings(
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-core" % previousIterateeVersion)
  )
  .jsSettings(commonJsSettings: _*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val testing = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("testing"))
  .settings(
    moduleName := "iteratee-testing",
    name := "testing"
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersionFor(scalaVersion.value),
      "org.scalatest" %%% "scalatest" % scalaTestVersionFor(scalaVersion.value),
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline" % disciplineVersionFor(scalaVersion.value)
    ),
    coverageExcludedPackages := "io\\.iteratee\\.testing\\..*"
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(core)

lazy val testingJVM = testing.jvm
lazy val testingJS = testing.js

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-tests",
    name := "tests"
  )
  .settings(allSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(inConfig(IntegrationTest)(scalafmtConfigSettings))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersionFor(scalaVersion.value),
      "org.scalatest" %%% "scalatest" % scalaTestVersionFor(scalaVersion.value),
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline" % disciplineVersionFor(scalaVersion.value)
    ),
    scalacOptions ~= {
      _.filterNot(Set("-Yno-predef"))
    },
    parallelExecution in Test := true,
    testForkedParallel in Test := true,
    parallelExecution in IntegrationTest := true,
    testForkedParallel in IntegrationTest := true,
    coverageExcludedPackages := "io\\.iteratee\\.tests\\..*"
  )
  .jvmSettings(
    fork := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect-laws" % catsEffectVersion % "it"
    )
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(testing)

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val files = project
  .settings(
    moduleName := "iteratee-files",
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-files" % previousIterateeVersion),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    )
  )
  .settings(allSettings)
  .dependsOn(coreJVM)

lazy val scalaz = project
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-scalaz",
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-scalaz" % previousIterateeVersion)
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(inConfig(IntegrationTest)(scalafmtConfigSettings))
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion
  )
  .dependsOn(coreJVM, files, testsJVM % "test,it")

lazy val monix = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("monix"))
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-monix",
    crossScalaVersions := crossScalaVersions.value.init
  )
  .settings(allSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(inConfig(IntegrationTest)(scalafmtConfigSettings))
  .settings(
    libraryDependencies ++= Seq(
      "io.monix" %%% "monix-eval" % monixVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion
    )
  )
  .jvmSettings(
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-monix" % previousIterateeVersion)
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(core, tests % "test,it")

lazy val monixJVM = monix.jvm
lazy val monixJS = monix.js

lazy val benchmark = project
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-benchmark"
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(inConfig(IntegrationTest)(scalafmtConfigSettings))
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
      "org.scalaz.stream" %% "scalaz-stream" % scalazStreamVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM, monixJVM, scalaz, testsJVM)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/travisbrown/iteratee")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  autoAPIMappings := true,
  apiURL := Some(url("https://travisbrown.github.io/iteratee/api/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/travisbrown/iteratee"),
      "scm:git:git@github.com:travisbrown/iteratee.git"
    )
  ),
  developers := List(
    Developer(
      "travisbrown",
      "Travis Brown",
      "travisrobertbrown@gmail.com",
      url("https://twitter.com/travisbrown")
    )
  ),
  pomPostProcess := { (node: XmlNode) =>
    new RuleTransformer(
      new RewriteRule {
        private def isTestScope(elem: Elem): Boolean =
          elem.label == "dependency" && elem.child.exists(child => child.label == "scope" && child.text == "test")

        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case elem: Elem if isTestScope(elem) => Nil
          case _                               => node
        }
      }
    ).transform(node).head
  }
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

credentials ++= (
  for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield
    Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      username,
      password
    )
).toSeq

val jvmProjects = Seq(
  "benchmark",
  "core",
  "files",
  "monix",
  "scalaz",
  "testing",
  "tests"
)

val jsProjects = Seq(
  "coreJS",
  "monixJS",
  "testingJS",
  "testsJS"
)

addCommandAlias("testJVM", jvmProjects.map(";" + _ + "/test").mkString)
addCommandAlias(
  "validateJVM",
  ";testJVM;tests/it:test;benchmark/it:test;monix/it:test;scalaz/it:test;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;it:scalafmtCheck;scalastyle;unidoc"
)
addCommandAlias("testJS", jsProjects.map(";" + _ + "/test").mkString)
addCommandAlias(
  "validateJS",
  ";testJS;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;it:scalafmtCheck;scalastyle;unidoc"
)
addCommandAlias("validate", ";validateJVM;validateJS")
