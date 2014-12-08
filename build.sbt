name := "scaladn"

scalaVersion := "2.11.2"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  //"com.chuusai" 	%% "shapeless" % "2.1.0-SNAPSHOT" changing(),
  //"org.scalaz"    %% "scalaz-core"    % "7.1.0-M7",
  "org.parboiled"   %% "parboiled"        % "2.0.1",
  "org.scalatest"   %% "scalatest"        % "2.2.1"             % "test"
)

//resolvers ++= Seq(
//  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
//  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//)


//resolvers ++= Seq(
//  Resolver.sonatypeRepo("releases"),
//  Resolver.sonatypeRepo("snapshots")
//)

scalacOptions ++= Seq("-unchecked", "-deprecation" /*, "-Xlog-implicits"*/)

fork in test := true

//javaOptions in test += "-Xmx4G"

