name := "german-analysis"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6"
libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.9"

scalafmtOnCompile in ThisBuild := true
