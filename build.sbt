import ReleaseTransformations._
import org.scalajs.sbtplugin.cross.{ CrossProject, CrossType }
import scala.xml.{ Elem, Node => XmlNode, NodeSeq => XmlNodeSeq }
import scala.xml.transform.{ RewriteRule, RuleTransformer }

organization in ThisBuild := "io.iteratee"

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

lazy val catsVersion = "1.0.0"
lazy val disciplineVersion = "0.8"
lazy val monixVersion = "2.3.2"
lazy val fs2Version = "0.10.0-M10"

lazy val scalaCheckVersion = "1.13.5"
lazy val scalaTestVersion = "3.0.4"

lazy val previousIterateeVersion = "0.15.0"

def crossModule(path: String, crossType: CrossType = CrossType.Full) = {
  val id = path.split("-").reduce(_ + _.capitalize)

  CrossProject(jvmId = id, jsId = id + "JS", file(path), crossType)
}

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

lazy val baseSettings = Seq(
  scalacOptions ++= (compilerOptions :+ "-Yno-predef") ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, p)) if p >= 11 => Seq("-Ywarn-unused-import")
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
  coverageHighlighting := (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => false
      case _ => true
    }
  ),
  (scalastyleSources in Compile) ++= (sourceDirectories in Compile).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.5" cross CrossVersion.binary)
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
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:travisbrown/iteratee.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inAnyProject -- inProjects(coreJS, benchmark, monixJS, testingJS, tests, testsJS)
)

lazy val iteratee = project.in(file("."))
  .enablePlugins(GhpagesPlugin, ScalaUnidocPlugin)
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(benchmark, core, coreJS, files, monix, monixJS, scalaz, testing, testingJS, tests, testsJS)
  .dependsOn(core, scalaz)

lazy val coreBase = crossModule("core", CrossType.Pure)
  .settings(
    moduleName := "iteratee-core",
    name := "core",
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
  )
  .jvmSettings(
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-core" % previousIterateeVersion)
  )
  .jsSettings(commonJsSettings: _*)

lazy val core = coreBase.jvm
lazy val coreJS = coreBase.js

lazy val testingBase = crossModule("testing")
  .settings(
    moduleName := "iteratee-testing",
    name := "testing"
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion,
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline" % disciplineVersion
    ),
    coverageExcludedPackages := "io\\.iteratee\\.testing\\..*"
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(coreBase)

lazy val testing = testingBase.jvm
lazy val testingJS = testingBase.js

lazy val testsBase = crossModule("tests")
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-tests",
    name := "tests",
  )
  .settings(allSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion,
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "discipline" % disciplineVersion
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
  .jvmSettings(fork := false)
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(testingBase)

lazy val tests = testsBase.jvm
lazy val testsJS = testsBase.js

lazy val files = project
  .settings(
    moduleName := "iteratee-files",
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-files" % previousIterateeVersion)
  )
  .settings(allSettings)
  .dependsOn(core)

lazy val scalaz = project
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-scalaz",
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-scalaz" % previousIterateeVersion)
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.17"
  ).dependsOn(core, files, tests % "test,it")

lazy val monixBase = crossModule("monix")
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-monix"
  )
  .settings(allSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    libraryDependencies += "io.monix" %%% "monix-eval" % monixVersion
  )
  .jvmSettings(
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-monix" % previousIterateeVersion)
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.dependsOn(files))
  .dependsOn(coreBase, testsBase % "test,it")

lazy val monix = monixBase.jvm
lazy val monixJS = monixBase.js

lazy val benchmark = project
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-benchmark"
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.scalaz" %% "scalaz-iteratee" % "7.2.16",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a",
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core, monix, scalaz, tests)

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
  ),
  pomPostProcess := { (node: XmlNode) =>
    new RuleTransformer(
      new RewriteRule {
        private def isTestScope(elem: Elem): Boolean =
          elem.label == "dependency" && elem.child.exists(child => child.label == "scope" && child.text == "test")

        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case elem: Elem if isTestScope(elem) => Nil
          case _ => node
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
addCommandAlias("validateJVM", ";testJVM;tests/it:test;benchmark/it:test;monix/it:test;scalaz/it:test;scalastyle;unidoc")
addCommandAlias("testJS", jsProjects.map(";" + _ + "/test").mkString)
addCommandAlias("validateJS", ";testJS;scalastyle;unidoc")
addCommandAlias("validate", ";validateJVM;validateJS")
