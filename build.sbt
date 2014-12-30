import com.typesafe.sbt.SbtGit._

import sbtunidoc.Plugin._

name in ThisBuild := "scaledn"

organization in ThisBuild := "com.mandubian"

scalaVersion in ThisBuild := "2.11.4"

version in ThisBuild := "1.0.0-SNAPSHOT"

libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest"   %% "scalatest"        % "2.2.1"             % "test"
  //"com.chuusai" 	%% "shapeless" % "2.1.0-SNAPSHOT" changing(),
  //"org.scalaz"    %% "scalaz-core"    % "7.1.0-M7",
)

resolvers in ThisBuild ++= Seq(
  "typesafe releases" at "https://repo.typesafe.com/typesafe/releases",
  "typesafe snapshots" at "https://repo.typesafe.com/typesafe/snapshots",
  "JTO snapshots" at "https://raw.github.com/jto/mvn-repo/master/snapshots"
)


scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation" /*, "-Xlog-implicits"*/)

fork in test := true

//javaOptions in test += "-Xmx4G"

lazy val root = (project in file("."))
  .settings  (publish := { })
  .settings  (unidocSettings: _*)
  .settings  (site.settings ++ ghpages.settings: _*)
  .settings  (
    site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "latest/api"),
    git.remoteRepo := "git@github.com:mandubian/scaledn.git"
  )
  .aggregate (common, parser, macros, validation)

lazy val common = project
  .settings(
    name := "scaledn-common",
    publishMavenStyle := true
  )
  .settings(bintraySettings:_*)

lazy val parser = project
  .settings(
    name := "scaledn-parser",
    libraryDependencies ++= Seq(
      "org.parboiled"   %% "parboiled"        % "2.0.1",
      "joda-time"        % "joda-time"        % "2.6",
      "org.joda"         % "joda-convert"     % "1.2"
    ),
    publishMavenStyle := true
  )
  .settings(bintraySettings:_*)
  .dependsOn (common)

lazy val validation = project
  .settings(
    name := "scaledn-validation",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies ++= Seq(
      "io.github.jto" %% "validation-core" % "1.0-1c770f4" excludeAll (
        ExclusionRule(organization = "com.typesafe.play")
      ),
      "com.typesafe.play" %% "play-functional" % "2.3.7",
      "com.typesafe.play" %% "play-json" % "2.3.7",
      "com.chuusai"       %% "shapeless" % "2.1.0-SNAPSHOT" changing()
    ),
    publishMavenStyle := true
  )
  .settings(bintraySettings:_*)
  .dependsOn (common, parser % "test->test", macros % "test->test")

lazy val macros = project
  .settings(
    name := "scaledn-macros",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    publishMavenStyle := true
  )
  .settings(bintraySettings:_*)
  .dependsOn (parser)

lazy val sampleHelloEDN = project in file("samples/helloedn")

versionWithGit

git.baseVersion := "1.0.0"

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/"))

publishMavenStyle in ThisBuild := true

bintrayPublishSettings

//resolvers ++= Seq(
//  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
//  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//)


//resolvers in ThisBuild ++= Seq(
//  Resolver.sonatypeRepo("releases"),
//  Resolver.sonatypeRepo("snapshots")
//)

