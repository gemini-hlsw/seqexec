import Settings._
import Settings.Libraries._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  ScalaZCore.value,
  ScalaZConcurrent,
  ScalaZStream,
  Argonaut,
  CommonsHttp,
  Squants.value,
  // OCS bundles
  SpModelCore,
  SeqexecOdb,
  POT,
  EpicsACM,
  Knobs
) ++ TestLibs.value
