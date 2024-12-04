val scala3Version = "3.3.3"

ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "com.onesockpirates"

val commonDeps = Seq(
  "org.scalameta"              %% "munit"         % "1.0.0" % Test,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

lazy val root = project
  .in(file("."))
  .settings(
    name                                                   := "functionalscala",
    libraryDependencies ++= commonDeps,
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.3.18"
  )
