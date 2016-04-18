import Settings._
import Settings.Libraries._

name := "edu.gemini.seqexec.shared"

libraryDependencies ++= Seq(
  ScalaZCore.value,
  SpModelCore,
  POT,
  TRPC
)
