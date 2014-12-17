name := "scaladn"

scalaVersion in ThisBuild := "2.11.2"

version := "1.0-SNAPSHOT"

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


scalacOptions ++= Seq("-unchecked", "-deprecation" /*, "-Xlog-implicits"*/)

fork in test := true

//javaOptions in test += "-Xmx4G"

lazy val root = (project in file(".")).aggregate (common, parser, validation)

lazy val common = project

lazy val parser = project
  .settings(
    libraryDependencies ++= Seq(
      "org.parboiled"   %% "parboiled"        % "2.0.1",
      "joda-time"        % "joda-time"        % "2.6",
      "org.joda"         % "joda-convert"     % "1.2"
    )
  ).dependsOn (common)

lazy val validation = project
  .settings(
    libraryDependencies ++= Seq(
      "io.github.jto" %% "validation-core" % "1.0-1c770f4" excludeAll (
        ExclusionRule(organization = "com.typesafe.play")
      ),
      "com.typesafe.play" %% "play-functional" % "2.3.7",
      "com.typesafe.play" %% "play-json" % "2.3.7",
      "com.chuusai"       %% "shapeless" % "2.1.0-SNAPSHOT" changing()
    )
  )
  .dependsOn (common, parser % "test->test")

lazy val macros = project.settings(
  libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
) dependsOn (parser, validation)

//resolvers ++= Seq(
//  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
//  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//)


//resolvers in ThisBuild ++= Seq(
//  Resolver.sonatypeRepo("releases"),
//  Resolver.sonatypeRepo("snapshots")
//)
