resolvers ++= Seq(
    Resolver.url(
      "bintray-sbt-plugin-releases",
      url("http://dl.bintray.com/content/sbt/sbt-plugin-releases")
    )(Resolver.ivyStylePatterns)
  , "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.1")
