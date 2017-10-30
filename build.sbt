import ReleaseTransformations._
import scala.xml.{ Elem, Node => XmlNode, NodeSeq => XmlNodeSeq }
import scala.xml.transform.{ RewriteRule, RuleTransformer }

organization in ThisBuild := "io.iteratee"

lazy val scalaVersions = Seq("2.10.6", "2.11.11", "2.12.4")

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

lazy val catsVersion = "1.0.0-MF"
lazy val disciplineVersion = "0.8"
lazy val monixVersion = "2.3.0"
lazy val fs2Version = "0.9.7"
lazy val fs2CatsVersion = "0.4.0"

lazy val scalaCheckVersion = "1.13.5"
lazy val scalaTestVersion = "3.0.4"

lazy val previousIterateeVersion = "0.13.0"

val docMappingsApiDir = settingKey[String]("Subdirectory in site target directory for API docs")

lazy val baseSettings = Seq(
  scalaVersion := "2.11.11",
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
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary)
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
    inAnyProject -- inProjects(coreJS, benchmark, monixJS, fs2JS, testingJS, tests, testsJS, twitter)
)

lazy val iteratee = project.in(file("."))
  .enablePlugins(CrossPerProjectPlugin, GhpagesPlugin, ScalaUnidocPlugin)
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(benchmark, core, coreJS, files, monix, monixJS, scalaz, fs2, testing, testingJS, tests, testsJS, twitter)
  .dependsOn(core, scalaz)

lazy val coreBase = crossProject.crossType(CrossType.Pure).in(file("core"))
  .enablePlugins(CrossPerProjectPlugin)
  .settings(
    moduleName := "iteratee-core",
    name := "core",
    crossScalaVersions := scalaVersions
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsVersion
  )
  .jvmSettings(
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-core" % previousIterateeVersion)
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "core"))
  .jsConfigure(_.copy(id = "coreJS"))

lazy val core = coreBase.jvm
lazy val coreJS = coreBase.js

lazy val testingBase = crossProject.in(file("testing"))
  .enablePlugins(CrossPerProjectPlugin)
  .settings(
    moduleName := "iteratee-testing",
    name := "testing",
    crossScalaVersions := scalaVersions
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
  .jvmConfigure(_.copy(id = "testing").dependsOn(files))
  .jsConfigure(_.copy(id = "testingJS"))
  .dependsOn(coreBase)

lazy val testing = testingBase.jvm
lazy val testingJS = testingBase.js

lazy val testsBase = crossProject.in(file("tests"))
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-tests",
    name := "tests",
    crossScalaVersions := scalaVersions
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
  .jvmConfigure(_.copy(id = "tests").dependsOn(files))
  .jsConfigure(_.copy(id = "testsJS"))
  .dependsOn(testingBase)

lazy val tests = testsBase.jvm
lazy val testsJS = testsBase.js

lazy val files = project
  .enablePlugins(CrossPerProjectPlugin)
  .settings(
    moduleName := "iteratee-files",
    crossScalaVersions := scalaVersions,
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-files" % previousIterateeVersion)
  )
  .settings(allSettings)
  .dependsOn(core)

lazy val twitter = project
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    crossScalaVersions := scalaVersions.tail,
    moduleName := "iteratee-twitter",
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-twitter" % previousIterateeVersion)
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(
    libraryDependencies += "io.catbird" %% "catbird-util" % "0.19.0"
  ).dependsOn(core, files, tests % "test,it")

lazy val scalaz = project
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-scalaz",
    crossScalaVersions := scalaVersions,
    mimaPreviousArtifacts := Set("io.iteratee" %% "iteratee-scalaz" % previousIterateeVersion)
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.16"
  ).dependsOn(core, files, tests % "test,it")

lazy val monixBase = crossProject.in(file("monix"))
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    moduleName := "iteratee-monix",
    crossScalaVersions := scalaVersions
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
  .jvmConfigure(_.copy(id = "monix").dependsOn(files))
  .jsConfigure(_.copy(id = "monixJS"))
  .dependsOn(coreBase, testsBase % "test,it")

lazy val monix = monixBase.jvm
lazy val monixJS = monixBase.js

lazy val fs2Base = crossProject.in(file("fs2"))
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    crossScalaVersions := scalaVersions.tail,
    moduleName := "iteratee-fs2"
  )
  .settings(allSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % fs2Version,
      "co.fs2" %%% "fs2-cats" % fs2CatsVersion
    )
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "fs2").dependsOn(files))
  .jsConfigure(_.copy(id = "fs2JS"))
  .dependsOn(coreBase, testsBase % "test,it")

lazy val fs2 = fs2Base.jvm
lazy val fs2JS = fs2Base.js

lazy val benchmark = project
  .enablePlugins(CrossPerProjectPlugin)
  .configs(IntegrationTest)
  .settings(
    crossScalaVersions := scalaVersions.tail,
    moduleName := "iteratee-benchmark"
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.scalaz" %% "scalaz-iteratee" % "7.2.16",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a",
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core, monix, scalaz, fs2, tests, twitter)

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
  publish := (),
  publishLocal := (),
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
  "fs2",
  "twitter",
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
addCommandAlias("validateJVM", ";testJVM;tests/it:test;benchmark/it:test;monix/it:test;scalaz/it:test;fs2/it:test;twitter/it:test;scalastyle;unidoc")
addCommandAlias("testJS", jsProjects.map(";" + _ + "/test").mkString)
addCommandAlias("validateJS", ";testJS;scalastyle;unidoc")
addCommandAlias("validate", ";validateJVM;validateJS")
