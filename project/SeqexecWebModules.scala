import sbt.Keys._
import sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import spray.revolver.RevolverPlugin.autoImport._

import Settings.Libraries._
import Settings.LibraryVersions

/**
  * Defines the modules used for the Seqexec Web front end
  */
trait SeqexecWebModules extends SeqexecEngineModules {
  // Root web project
  lazy val edu_gemini_seqexec_web = project.in(file("modules/edu.gemini.seqexec.web"))
    .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

  lazy val commonSettings = Seq(
    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value, UPickle.value, BooPickle.value) ++ TestLibs.value
  )

  // a special crossProject for configuring a JS/JVM/shared structure
  lazy val edu_gemini_seqexec_web_shared = (crossProject.crossType(CrossType.Pure) in file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.shared"))
    .settings(commonSettings: _*)
    .dependsOn(edu_gemini_seqexec_model)
    .jvmSettings(
    )
    .jsSettings(
      libraryDependencies += JavaLogJS.value
    )

  lazy val edu_gemini_seqexec_web_shared_JVM = edu_gemini_seqexec_web_shared.jvm

  lazy val edu_gemini_seqexec_web_shared_JS = edu_gemini_seqexec_web_shared.js

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

  // Project for the server side application
  lazy val edu_gemini_seqexec_web_server = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.server"))
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(UnboundId, JwtCore, Slf4jJuli, Knobs) ++ Http4s,

      // Settings to optimize the use of sbt-revolver

      // Allows to read the generated JS on client
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.map((x: sbt.File) => new File(x.getAbsolutePath + ".map")).data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in(edu_gemini_seqexec_web_client, Compile, packageJSDependencies)).value,
      // Support stopping the running server
      mainClass in reStart := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
      // do a fastOptJS on reStart
      reStart <<= reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client, Compile)),
      // This settings makes reStart to rebuild if a scala.js file changes on the client
      watchSources ++= (watchSources in edu_gemini_seqexec_web_client).value,
      // On recompilation only consider changes to .scala and .js files
      watchSources ~= { t:Seq[java.io.File] => {t.filter(includeInTrigger)} },

      // Settings for the command line client on scala.js
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client_cli, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client_cli, Compile)).value.map((x: sbt.File) => new File(x.getAbsolutePath + ".map")).data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in(edu_gemini_seqexec_web_client_cli, Compile, packageJSDependencies)).value,
      // do a fastOptJS on reStart
      reStart <<= reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client_cli, Compile))
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoKeys += buildInfoBuildNumber,
      buildInfoOptions += BuildInfoOption.BuildTime,
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.seqexec.web.server"
    )
    .dependsOn(edu_gemini_seqexec_web_shared_JVM, edu_gemini_seqexec_server)

  // Client side project using Scala.js
  lazy val edu_gemini_seqexec_web_client = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client"))
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
      artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "seqexec.js",
      artifactPath in (Compile, fullOptJS) := (resourceManaged in Compile).value / "seqexec-opt.js",
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
        JQuery.value,
        ScalaCSS.value,
        ScalaJSDom.value,
        JavaTimeJS.value,
        JavaLogJS.value
      ) ++ ReactScalaJS.value ++ Diode.value
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.seqexec.web.client"
    )
    .dependsOn(edu_gemini_seqexec_web_shared_JS % "compile->compile;test->test", edu_gemini_seqexec_model_JS % "compile->compile;test->test")

  // Client side project using Scala.js
  lazy val edu_gemini_seqexec_web_client_cli = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client.cli"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      // Write the generated js to the filename seqexec-cli.js
      artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "seqexec-cli.js",
      artifactPath in (Compile, fullOptJS) := (resourceManaged in Compile).value / "seqexec-cli-opt.js",
      // Requires the DOM
      jsDependencies += RuntimeDOM,
      // JS dependencies from webjars
      jsDependencies ++= Seq(
        "org.webjars" % "jquery"          % LibraryVersions.jQuery         / "jquery.js"            minified "jquery.min.js",
        "org.webjars" % "jquery.terminal" % LibraryVersions.jQueryTerminal / "jquery.terminal.js"   minified "jquery.terminal.min.js" dependsOn "jquery.js"
      ),
      // Build a js dependencies file
      skip in packageJSDependencies := false,
      emitSourceMaps in fullOptJS := true,
      // Put the jsdeps file on a place reachable for the server
      crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
      // Compile tests to JS using fast-optimisation
      scalaJSStage in Test := FastOptStage,
      libraryDependencies ++= Seq(
        JQuery.value
      )
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.seqexec.web.client.cli"
    )
    .dependsOn(edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_model_JS)

}
