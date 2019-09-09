import ReleaseTransformations._
import org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings
import sbtcrossproject.{ CrossType, crossProject }
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

val catsVersion = "2.0.0"
val catsEffectVersion = "2.0.0"
val scalazVersion = "7.2.28"
val fs2Version = "1.1.0-M1"

val scalaTestVersion = "3.1.0-SNAP13"
val scalaCheckVersion = "1.14.0"
val disciplineVersion = "1.0.0"

/**
 * Some terrible hacks to work around Cats's decision to have builds for
 * different Scala versions depend on different versions of Discipline, etc.
 */
def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val previousIterateeVersion = "0.18.0"

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

lazy val baseSettings = Seq(
  scalacOptions ++= {
    if (priorTo2_13(scalaVersion.value)) compilerOptions
    else
      compilerOptions.flatMap {
        case "-Ywarn-unused-import" => Some("-Ywarn-unused:imports")
        case "-Yno-adapted-args"    => None
        case "-Xfuture"             => None
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
        case "-Xfuture"             => None
        case other                  => Some(other)
      }
  },
  scalacOptions in (Compile, test) := {
    if (priorTo2_13(scalaVersion.value)) compilerOptions
    else
      compilerOptions.flatMap {
        case "-Ywarn-unused-import" => Some("-Ywarn-unused:imports")
        case "-Yno-adapted-args"    => None
        case "-Xfuture"             => None
        case other                  => Some(other)
      }
  },
  coverageHighlighting := true,
  (scalastyleSources in Compile) ++= (sourceDirectories in Compile).value,
  addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.10.3").cross(CrossVersion.binary))
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
    inAnyProject -- inProjects(coreJS, benchmark, testingJS, testsJVM, testsJS)
)

lazy val iteratee = project
  .in(file("."))
  .enablePlugins(GhpagesPlugin, ScalaUnidocPlugin)
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS, files, testingJVM, testingJS, testsJVM, testsJS, benchmark)
  .dependsOn(coreJVM, files)

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
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion,
      "org.scalatestplus" %%% "scalatestplus-scalacheck" % "3.1.0.0-RC2",
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline-core" % disciplineVersion
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
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion,
      "org.scalatestplus" %%% "scalatestplus-scalacheck" % "3.1.0.0-RC2",
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline-core" % disciplineVersion
    ),
    scalacOptions ~= {
      _.filterNot(Set("-Yno-predef"))
    },
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
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
      "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM, testsJVM)

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
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
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
  } yield Credentials(
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
  "testing",
  "tests"
)

val jsProjects = Seq(
  "coreJS",
  "testingJS",
  "testsJS"
)

addCommandAlias("testJVM", jvmProjects.map(";" + _ + "/test").mkString)
addCommandAlias(
  "validateJVM",
  ";testJVM;tests/it:test;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;it:scalafmtCheck;scalastyle;unidoc"
)
addCommandAlias("testJS", jsProjects.map(";" + _ + "/test").mkString)
addCommandAlias(
  "validateJS",
  ";testJS;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;it:scalafmtCheck;scalastyle;unidoc"
)
addCommandAlias("validate", ";validateJVM;validateJS")
