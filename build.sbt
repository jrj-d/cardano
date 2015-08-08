name := "cardana"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"
 
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq (
	"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)
