name := "helloedn"

resolvers += bintray.Opts.resolver.mavenRepo("mandubian")

libraryDependencies ++= Seq(
  // only need scaledn parser?
    "com.mandubian" %% "scaledn-parser"     % "1.0.0-f77f98cc305ce8a304d8941f800505c6b3d41d74"
  // only need scaledn validation/serialization?
  , "com.mandubian" %% "scaledn-validation" % "1.0.0-f77f98cc305ce8a304d8941f800505c6b3d41d74"
  // only need scaledn macros?
  , "com.mandubian" %% "scaledn-macros"     % "1.0.0-f77f98cc305ce8a304d8941f800505c6b3d41d74"
)
