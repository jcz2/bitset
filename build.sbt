ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "bitset"
  )

libraryDependencies := Seq(
  "org.scalatest" %% "scalatest" % "3.2.14",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0"
)
