import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import sbt.Keys._
import sbt._
import Settings.Libraries._
import Settings.LibraryVersions
import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import spray.revolver.RevolverPlugin.autoImport._

/**
  * Defines the modules used for the Phase1 Bcakend
  */
trait Phase1BackendModules extends Common {
  // Root web project
  lazy val edu_gemini_p1backend = project.in(file("modules/edu.gemini.p1backend"))
    .aggregate(edu_gemini_p1backend_server, edu_gemini_p1backend_client, edu_gemini_p1backend_shared_JS, edu_gemini_p1backend_shared_JVM)

  // a special crossProject for configuring a JS/JVM/shared structure
  lazy val edu_gemini_p1backend_shared = (crossProject.crossType(CrossType.Pure) in file("modules/edu.gemini.p1backend/edu.gemini.p1backend.shared"))
    .settings(commonSettings: _*)

  lazy val edu_gemini_p1backend_shared_JVM = edu_gemini_p1backend_shared.jvm

  lazy val edu_gemini_p1backend_shared_JS = edu_gemini_p1backend_shared.js

  // Project for the server side application
  lazy val edu_gemini_p1backend_server = project.in(file("modules/edu.gemini.p1backend/edu.gemini.p1backend.server"))
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(Slf4jJuli, Knobs) ++ Http4s,

      // Settings to optimize the use of sbt-revolver

      // Allows to read the generated JS on client
      resources in Compile += (fastOptJS in (edu_gemini_p1backend_client, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (edu_gemini_p1backend_client, Compile)).value.map((x: sbt.File) => new File(x.getAbsolutePath + ".map")).data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in(edu_gemini_p1backend_client, Compile, packageJSDependencies)).value,
      // Support stopping the running server
      mainClass in reStart := Some("edu.gemini.p1backend.server.http4s.WebServerLauncher"),
      // do a fastOptJS on reStart
      reStart <<= reStart dependsOn (fastOptJS in (edu_gemini_p1backend_client, Compile)),
      // This settings makes reStart to rebuild if a scala.js file changes on the client
      watchSources ++= (watchSources in edu_gemini_p1backend_client).value,
      // On recompilation only consider changes to .scala and .js files
      watchSources ~= { t:Seq[java.io.File] => {t.filter(includeInTrigger)} }
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoKeys += buildInfoBuildNumber,
      buildInfoOptions += BuildInfoOption.BuildTime,
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.p1backend.server"
    )
    .dependsOn(edu_gemini_p1backend_shared_JVM)

  // Client side project using Scala.js
  lazy val edu_gemini_p1backend_client = project.in(file("modules/edu.gemini.p1backend/edu.gemini.p1backend.client"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      // This is a not very nice trick to remove js files that exist on the scala tools
      // library and that conflict with the requested on jsDependencies, in particular
      // with jquery.js
      // See http://stackoverflow.com/questions/35374131/scala-js-missing-js-library, UPDATE #1
      (scalaJSNativeLibraries in Test) <<= (scalaJSNativeLibraries in Test).map { l =>
        l.map(virtualFiles => virtualFiles.filter(vf => {
          val f = vf.toURI.toString
          !(f.endsWith(".js") && f.contains("scala/tools"))
        }))
      },
      // Write the generated js to the filename seqexec.js
      artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "p1backend.js",
      artifactPath in (Compile, fullOptJS) := (resourceManaged in Compile).value / "p1backend-opt.js",
      // Requires the DOM
      jsDependencies += RuntimeDOM,
      // JS dependencies from webjars
      jsDependencies ++= Seq(
        "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
        "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-dom.js"         minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
        "org.webjars"       % "jquery"      % LibraryVersions.jQuery      / "jquery.js"            minified "jquery.min.js" commonJSName "jQuery",
        "org.webjars"       % "Semantic-UI" % LibraryVersions.semanticUI  / "semantic.js"          minified "semantic.min.js" dependsOn "jquery.js"
      ),
      // Build a js dependencies file
      skip in packageJSDependencies := false,
      // Put the jsdeps file on a place reachable for the server
      crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
      libraryDependencies ++= Seq(
        JQuery.value
      ) ++ ReactScalaJS.value ++ Diode.value
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.p1backend.client"
    )
    .dependsOn(edu_gemini_p1backend_shared_JS % "compile->compile;test->test")

}
