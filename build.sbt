name := "cardano"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.0"

resolvers ++= Seq(
            "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq (
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

initialCommands in console :=
  """
    |def time[R](block: => R): R = {
    |    val t0 = System.nanoTime()
    |    val result = block    // call-by-name
    |    val t1 = System.nanoTime()
    |    println("Elapsed time: " + (t1 - t0)/1e9 + "s")
    |    result
    |}
    |
    |import cardano._
  """.stripMargin

// scalastyle for main code

lazy val compileScalaStyle = taskKey[Unit]("compileScalaStyle")

compileScalaStyle := {
  org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value
}

(compile in Compile) <<= (compile in Compile) dependsOn compileScalaStyle

// scalastyle for tests

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"

lazy val testScalaStyle = taskKey[Unit]("testScalaStyle")

testScalaStyle := {
  org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Test).toTask("").value
}

(test in Test) <<= (test in Test) dependsOn testScalaStyle
