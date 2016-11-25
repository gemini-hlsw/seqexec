import Settings._
import Settings.Libraries._

name := "edu.gemini.seqexec.engine"

libraryDependencies ++= Seq(
  ScalaZCore.value,
  ScalaZConcurrent,
  ScalaZStream
) ++ TestLibs.value
