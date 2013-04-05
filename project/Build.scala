import sbt._
import sbt.Keys._

object ProjectBuild extends Build {
  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "german-analysis",
      organization := "org.bruchez",
      version := "0.2",
      scalaVersion := "2.10.1",
      libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.9"
      // add other settings here
    )
  )
}
