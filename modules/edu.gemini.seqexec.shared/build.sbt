import Settings._
import Settings.Libraries._
import Settings.LibrariesJVM._

name := "edu.gemini.seqexec.shared"

libraryDependencies ++= Seq(
  ScalaZCore,
  SpModelCore,
  POT,
  TRPC
)
