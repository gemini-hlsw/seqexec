import sbt.Keys._

name := Settings.Definitions.name

scalaVersion in ThisBuild := Settings.LibraryVersions.scala

scalacOptions in ThisBuild ++= Settings.Definitions.scalacOptions

organization in Global := "edu.gemini.ocs"

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"

// Bintray for knobs
resolvers in ThisBuild += Resolver.bintrayRepo("oncue", "releases")

// This key is used to find the JRE dir. It could/should be overriden on a user basis
// Add e.g. a `jres.sbt` file with your particular configuration
ocsJreDir in ThisBuild := Path.userHome / ".jres8"
