ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "com.com.example"

lazy val hello = (project in file("."))
  .settings(name := "Aoc2021")


libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"