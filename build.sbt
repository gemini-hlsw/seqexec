
name := Settings.Definitions.name

version := Settings.Definitions.version

scalaVersion in ThisBuild := Settings.LibraryVersions.scala

organization in Global := "edu.gemini.ocs"

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "http://sbfswgosxdev-mp1.cl.gemini.edu:8081/artifactory/libs-release-local"

