import Settings._

name := "edu.gemini.seqexec.server"

libraryDependencies ++= Seq(
  "org.scalaz"         %% "scalaz-core"               % "7.1.6",
  "org.scalaz"         %% "scalaz-concurrent"         % "7.1.6",
  "com.squants"        %% "squants"                   % "0.5.3",  // This needs to be replaced by our custom version
  "io.argonaut"        %% "argonaut"                  % "6.1",
  "commons-httpclient" % "commons-httpclient"         % "2.0",
  // OCS bundles
  "edu.gemini.ocs"     %% "edu-gemini-spmodel-core"   % LibraryVersions.ocsVersion,
  "edu.gemini.ocs"     %% "edu-gemini-pot"            % LibraryVersions.ocsVersion,
  "edu.gemini.ocs"     %% "edu-gemini-epics-acm"      % LibraryVersions.ocsVersion
)
