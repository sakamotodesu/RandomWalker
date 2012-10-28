import sbt._
import sbt.Keys._

object RandomwalkerBuild extends Build {

  lazy val randomwalker = Project(
    id = "randomwalker",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "RandomWalker",
      organization := "com.github.sakamotodesu.randomwalker",
      version := "0.2-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies ++= Seq("com.typesafe.akka" % "akka-actor" % "2.0.1",
                                   "org.specs2" %% "specs2" % "latest.integration" % "test"),
      scalacOptions ++= Seq("-deprecation", "-Xmigration")
    )
  )
}
