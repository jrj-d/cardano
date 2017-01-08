name := "cardano"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
            "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq (
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
	"org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"
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
