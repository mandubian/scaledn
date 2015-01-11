name := "helloedn"

resolvers += bintray.Opts.resolver.mavenRepo("mandubian")

val scalednVersion = "1.0.0-e8180d08620a607ec47613f8c2585f7784e86625"

libraryDependencies ++= Seq(
  // only need scaledn parser?
    "com.mandubian" %% "scaledn-parser"     % scalednVersion
  // only need scaledn validation/serialization?
  , "com.mandubian" %% "scaledn-validation" % scalednVersion
  // only need scaledn macros?
  , "com.mandubian" %% "scaledn-macros"     % scalednVersion
)
