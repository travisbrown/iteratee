import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization := "io.iteratee",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.6", "2.11.7")
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

lazy val catsVersion = "0.4.0-SNAPSHOT"
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
  ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => false
      case _ => true
    }
  )
)

lazy val allSettings = buildSettings ++ baseSettings ++ publishSettings

lazy val commonJsSettings = Seq(
  postLinkJSEnv := NodeJSEnv().value,
  scalaJSStage in Global := FastOptStage
)

lazy val docSettings = site.settings ++ ghpages.settings ++ unidocSettings ++ Seq(
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq("-groups", "-implicits"),
  git.remoteRepo := "git@github.com:travisbrown/iteratee.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inAnyProject -- inProjects(coreJS)
)

lazy val root = project.in(file("."))
  .settings(allSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .aggregate(core, coreJS, task)
  .dependsOn(core)

lazy val coreBase = crossProject.in(file("core"))
  .settings(
    moduleName := "iteratee-core",
    name := "core",
    testOptions in Test ++= (
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) => Seq(Tests.Argument("-l", "io.iteratee.NoScala210Test"))
        case _ => Nil
      }
    )
  )
  .settings(allSettings: _*)
  .settings(
    libraryDependencies += "org.spire-math" %%% "cats-core" % catsVersion,
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.5",
      "org.scalatest" %% "scalatest" % scalaTestVersion,
      "org.spire-math" %% "cats-free" % catsVersion,
      "org.spire-math" %% "cats-laws" % catsVersion,
      "org.typelevel" %% "discipline" % "0.4"
    ).map(_ % "test")
  )
  .jsSettings(commonJsSettings: _*)
  .jvmConfigure(_.copy(id = "core"))
  .jsConfigure(_.copy(id = "coreJS"))

lazy val core = coreBase.jvm
lazy val coreJS = coreBase.js

lazy val task = project
  .settings(
    moduleName := "iteratee-task"
  )
  .settings(allSettings)
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.0"
  ).dependsOn(core)

lazy val benchmark = project
  .settings(moduleName := "iteratee-benchmark")
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-iteratee" % "7.1.5",
      "org.scalaz.stream" %% "scalaz-stream" % "0.8",
      "org.spire-math" %% "cats-free" % catsVersion
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core)

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
  "task"
)

val jsProjects = Seq(
  "coreJS"
)

addCommandAlias("buildJVM", jvmProjects.map(";" + _ + "/compile").mkString)
addCommandAlias("validateJVM", ";buildJVM;core/test;task/test;scalastyle;unidoc")
addCommandAlias("buildJS", jsProjects.map(";" + _ + "/compile").mkString)
addCommandAlias("validateJS", ";buildJS;coreJS/test;scalastyle")
addCommandAlias("validate", ";validateJVM;validateJS")
