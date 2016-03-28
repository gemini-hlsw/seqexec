import Settings._
import Settings.Libraries._
import Settings.LibrariesJVM._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  ScalaZCore,
  ScalaZConcurrent,
  Argonaut,
  CommonsHttp,
  // OCS bundles
  SpModelCore,
  POT,
  EpicsACM
)

unmanagedJars in Compile ++= Seq(
  new File((baseDirectory in ThisBuild).value, "lib/squants_2.11-0.6.1.jar")
)