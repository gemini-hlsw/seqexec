import Settings._
import Settings.Libraries._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  ScalaZCore.value,
  ScalaZConcurrent,
  Argonaut,
  CommonsHttp,
  Squants.value,
  // OCS bundles
  SpModelCore,
  POT,
  EpicsACM
) ++ TestLibs.value
