import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.gemini",
      scalaVersion := "2.12.1",
      scalacOptions ++= Seq(
        "-feature",
        "-language:higherKinds",
        "-language:existentials"
      ),
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "engine",
    libraryDependencies ++= Seq(
      cats,
      catsEffect,
      fs2,
      monocle,
      monocleMacro,
      scalaTest % Test
    )
  )
