name := "scaladn"

scalaVersion := "2.11.2"

version := "1.0-SNAPSHOT"

libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest"   %% "scalatest"        % "2.2.1"             % "test"
  //"com.chuusai" 	%% "shapeless" % "2.1.0-SNAPSHOT" changing(),
  //"org.scalaz"    %% "scalaz-core"    % "7.1.0-M7",
)

scalacOptions ++= Seq("-unchecked", "-deprecation" /*, "-Xlog-implicits"*/)

fork in test := true

//javaOptions in test += "-Xmx4G"

lazy val root = (project in file("."))
  .aggregate(parser, validation)

lazy val parser = project
  .settings(
    libraryDependencies ++= Seq(
      "org.parboiled"   %% "parboiled"        % "2.0.1",
      "joda-time"        % "joda-time"        % "2.6",
      "org.joda"         % "joda-convert"     % "1.2"
    )
  )

lazy val validation = project
  .settings(
    resolvers += "JTO snapshots" at "https://raw.github.com/jto/mvn-repo/master/snapshots"
  )
  .settings(
    libraryDependencies += "io.github.jto" %% "validation-core" % "1.0-1c770f4"
  )

//resolvers ++= Seq(
//  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
//  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//)


//resolvers ++= Seq(
//  Resolver.sonatypeRepo("releases"),
//  Resolver.sonatypeRepo("snapshots")
//)
