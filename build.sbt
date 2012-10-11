import AssemblyKeys._

assemblySettings

name := "RandomWalker"

version := "1.1"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

scalacOptions += "-Xmigration"

libraryDependencies += "org.specs2" %% "specs2" % "latest.integration" % "test"
