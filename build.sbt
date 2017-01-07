name := "cardano"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
            "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq (
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
	"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)
