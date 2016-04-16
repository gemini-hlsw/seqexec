import Settings._
import Settings.Libraries._
import Settings.LibrariesJVM._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  ScalaZCore,
  ScalaZConcurrent,
  Argonaut,
  CommonsHttp,
  Squants.value,
  // OCS bundles
  SpModelCore,
  POT,
  EpicsACM
) ++ TestLibs.value
