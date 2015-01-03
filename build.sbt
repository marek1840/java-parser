name := "java-parser"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
"org.parboiled" %% "parboiled" % "2.0.1",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
)
