import Settings._
import Settings.Libraries._
import Settings.LibrariesJVM._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  ScalaZCore,
  ScalaZConcurrent,
  "com.squants"        %% "squants"                   % "0.5.3",  // This needs to be replaced by our custom version
  "io.argonaut"        %% "argonaut"                  % "6.1",
  "commons-httpclient" % "commons-httpclient"         % "2.0",
  // OCS bundles
  SpModelCore,
  POT,
  EpicsACM
)
