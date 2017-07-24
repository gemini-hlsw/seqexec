import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats" % "0.9.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "0.3"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "0.10.0-M2"
  lazy val monocle = "com.github.julien-truffaut" %% "monocle-core" % "1.4.0"
  lazy val monocleMacro = "com.github.julien-truffaut" %% "monocle-macro" % "1.4.0"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
}
