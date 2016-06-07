name := "SimpleGame"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.1"
libraryDependencies += "org.scalaz" % "scalaz-effect_2.11" % "7.2.1"
libraryDependencies += "org.scalaz" % "scalaz-concurrent_2.11" % "7.2.1"
libraryDependencies += "org.scalaz.stream" % "scalaz-stream_2.11" % "0.8.1a"

mainClass in Compile := Some("edu.gemini.seqexec.Main")
