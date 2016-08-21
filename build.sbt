import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._
import scala.xml.transform.{ RewriteRule, RuleTransformer }

lazy val buildSettings = Seq(
  organization := "io.iteratee",
  scalaVersion := "2.11.8"
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

val scalaVersions = Seq("2.10.6", "2.11.8")

lazy val catsVersion = "0.7.0"
lazy val disciplineVersion = "0.4"
lazy val scalaCheckVersion = "1.12.5"
lazy val scalaTestVersion = "3.0.0-M9"

lazy val baseSettings = Seq(
  scalacOptions ++= (compilerOptions :+ "-Yno-predef") ++ (
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
  ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := (
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

lazy val iteratee = project.in(file("."))
  .enablePlugins(CrossPerProjectPlugin)
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(benchmark, core, coreJS, files, scalaz, tests, testsJS, twitter)
  .dependsOn(core, scalaz)

lazy val coreBase = crossProject.crossType(CrossType.Pure).in(file("core"))
  .settings(
    crossScalaVersions := scalaVersions,
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
    crossScalaVersions := scalaVersions,
    moduleName := "iteratee-tests",
    name := "tests"
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
    testForkedParallel in IntegrationTest := true
  )
  .settings(
    ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "io\\.iteratee\\.tests\\..*",
    testOptions in Test ++= (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => Seq(Tests.Argument("-l", "io.iteratee.tests.NoScala210Test"))
        case _ => Nil
      }
    )
  )
  .jvmSettings(fork := false)
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "tests").dependsOn(files))
  .jsConfigure(_.copy(id = "testsJS"))
  .dependsOn(coreBase)

lazy val tests = testsBase.jvm
lazy val testsJS = testsBase.js

lazy val files = project
  .settings(
    crossScalaVersions := scalaVersions,
    moduleName := "iteratee-files"
  )
  .settings(allSettings)
  .dependsOn(core)

lazy val twitter = project
  .configs(IntegrationTest)
  .settings(
    crossScalaVersions := scalaVersions.tail,
    moduleName := "iteratee-twitter"
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(
    libraryDependencies += "io.catbird" %% "catbird-util" % "0.7.0"
  ).dependsOn(core, files, tests % "test,it")

lazy val scalaz = project
  .configs(IntegrationTest)
  .settings(
    crossScalaVersions := scalaVersions,
    moduleName := "iteratee-scalaz"
  )
  .settings(allSettings ++ Defaults.itSettings)
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.5"
  ).dependsOn(core, files, tests % "test,it")

lazy val benchmark = project
  .settings(
    crossScalaVersions := scalaVersions.tail,
    moduleName := "iteratee-benchmark"
  )
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "0.9.0-RC1",
      "com.typesafe.play" %% "play-iteratees" % "2.6.0",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.scalaz" %% "scalaz-iteratee" % "7.2.5",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8.3a"
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core, scalaz, twitter)

val removeScoverage = new RuleTransformer(
  new RewriteRule {
    private[this] def isGroupScoverage(child: xml.Node): Boolean =
      child.label == "groupId" && child.text == "org.scoverage"

    override def transform(node: xml.Node): Seq[xml.Node] = node match {
      case e: xml.Elem if e.label == "dependency" && e.child.exists(isGroupScoverage) => Nil
      case _ => Seq(node)
    }
  }
)

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
  pomPostProcess := { (node: xml.Node) => removeScoverage.transform(node).head }
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
  "scalaz",
  "twitter",
  "tests"
)

val jsProjects = Seq(
  "coreJS",
  "testsJS"
)

addCommandAlias("testJVM", jvmProjects.map(";" + _ + "/test").mkString)
addCommandAlias("validateJVM", ";testJVM;scalaz/it:test;twitter/it:test;scalastyle;unidoc")
addCommandAlias("testJS", jsProjects.map(";" + _ + "/test").mkString)
addCommandAlias("validateJS", ";testJS;scalastyle;unidoc")
addCommandAlias("validate", ";validateJVM;validateJS")
