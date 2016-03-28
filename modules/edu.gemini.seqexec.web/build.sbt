import Settings._
import Settings.LibrariesJVM._
import Settings.LibrariesJS._
import Settings.Libraries._

name := "edu.gemini.seqexec.web"

// Root web project
lazy val edu_gemini_seqexec_web = project.in(file("."))
  .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

lazy val commonSettings = Seq(
  // Common libraries
  libraryDependencies ++= UPickle.value +: TestLibs.value
  // TODO: Turn it on when the project goes scalaz 7.2.1
  //"org.scalaz"     %%% "scalaz-core" % "7.2.1",
)

// a special crossProject for configuring a JS/JVM/shared structure
lazy val edu_gemini_seqexec_web_shared = (crossProject.crossType(CrossType.Pure) in file("edu.gemini.seqexec.web.shared"))
  .settings(commonSettings: _*)
  .jvmSettings(
    libraryDependencies += ScalaZCore
  )
  .jsSettings(
    libraryDependencies += ScalaZCoreJS.value
  )

lazy val edu_gemini_seqexec_web_shared_JVM = edu_gemini_seqexec_web_shared.jvm

lazy val edu_gemini_seqexec_web_shared_JS = edu_gemini_seqexec_web_shared.js

// Client side project using Scala.js
lazy val edu_gemini_seqexec_web_client = project.in(file("edu.gemini.seqexec.web.client"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ScalaZCoreJS.value,
      ScalaCSS.value,
      ScalaJSDom.value
    ) ++ ReactScalaJS.value
  )
  .dependsOn(edu_gemini_seqexec_web_shared_JS)

// This function allows triggered compilation to run only when scala files changes
// It lets change static files freely
def includeInTrigger(f: java.io.File): Boolean =
  f.isFile && {
    val name = f.getName.toLowerCase
    name.endsWith(".scala") || name.endsWith(".js")
  }

// Project for the server side application
lazy val edu_gemini_seqexec_web_server = project.in(file("edu.gemini.seqexec.web.server"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= ScalaZCore +: (Http4s ++ Play),

    // Settings to optimize the use of sbt-revolver
    
    // Allows to read the generated JS on client
    resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
    // Support stopping the running server
    mainClass in reStart := Some("edu.gemini.seqexec.web.server.play.WebServerLauncher"),
    // do a fastOptJS on reStart
    reStart <<= reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client, Compile)),
    // This settings makes reStart to rebuild if a scala.js file changes on the client
    watchSources ++= (watchSources in edu_gemini_seqexec_web_client).value,
    // On recompilation only consider changes to .scala and .js files
    watchSources ~= { t:Seq[java.io.File] => {t.filter(includeInTrigger)} }

  )
  .dependsOn(edu_gemini_seqexec_web_shared_JVM, edu_gemini_seqexec_server)

