import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import sbt.Keys._
import sbt._
import Settings.Libraries._
import Settings.LibraryVersions
import org.scalajs.sbtplugin.ScalaJSPlugin
import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import spray.revolver.RevolverPlugin.autoImport._

object OcsBuild extends Build {

  // The variables defined at this level can be referred
  // by the build.sbt files at the module level

  // List all the modules and their inter dependencies
  lazy val edu_gemini_seqexec_server = project
    .in(file("modules/edu.gemini.seqexec.server"))
    .dependsOn(edu_gemini_seqexec_model_JVM)

  // Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
  // We have to define the project properties at this level
  lazy val edu_gemini_seqexec_model = crossProject.crossType(CrossType.Pure)
    .in(file("modules/edu.gemini.seqexec.model"))
    .settings(
      libraryDependencies ++= Seq(BooPickle.value) ++ TestLibs.value
    )
    .jsSettings(
      scalaJSUseRhino := false
    )

  lazy val edu_gemini_seqexec_model_JVM:Project = edu_gemini_seqexec_model.jvm

  lazy val edu_gemini_seqexec_model_JS:Project = edu_gemini_seqexec_model.js

  // Root web project
  lazy val edu_gemini_seqexec_web = project.in(file("modules/edu.gemini.seqexec.web"))
    .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

  lazy val commonSettings = Seq(
    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value, UPickle.value) ++ TestLibs.value
  )

  // a special crossProject for configuring a JS/JVM/shared structure
  lazy val edu_gemini_seqexec_web_shared = (crossProject.crossType(CrossType.Pure) in file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.shared"))
    .settings(commonSettings: _*)
    .jvmSettings(
    )
    .jsSettings(
      scalaJSUseRhino := false
    )

  lazy val edu_gemini_seqexec_web_shared_JVM = edu_gemini_seqexec_web_shared.jvm

  lazy val edu_gemini_seqexec_web_shared_JS = edu_gemini_seqexec_web_shared.js

  // Client side project using Scala.js
  lazy val edu_gemini_seqexec_web_client = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      // Skip tests in module, Rhino doesn't play nice with jquery
      test := {},
      // Write the generated js to the filename seqexec.js
      artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "seqexec.js",
      // JS dependencies from webjars
      jsDependencies ++= Seq(
        "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
        "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-dom.js"         minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
        "org.webjars"       % "jquery"      % LibraryVersions.jQuery      / "jquery.js"            minified "jquery.min.js",
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
        JavaTimeJS.value
      ) ++ ReactScalaJS.value ++ Diode.value
    )
    .settings(
      buildInfoUsePackageAsPath := true,
      buildInfoKeys := Seq(name, version),
      buildInfoObject := "OcsBuildInfo",
      buildInfoPackage := "edu.gemini.seqexec.web.client"
    )
    .dependsOn(edu_gemini_seqexec_web_shared_JS % "compile->compile;test->test", edu_gemini_seqexec_model_JS)

  // Client side project using Scala.js
  lazy val edu_gemini_seqexec_web_client_cli = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client.cli"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings: _*)
    .settings(
      // Skip tests in module, Rhino doesn't play nice with jquery
      test := {},
      // Write the generated js to the filename seqexec.js
      artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "seqexec-cli.js",
      // JS dependencies from webjars
      jsDependencies ++= Seq(
        "org.webjars" % "jquery"          % LibraryVersions.jQuery         / "jquery.js"            minified "jquery.min.js",
        "org.webjars" % "jquery.terminal" % LibraryVersions.jQueryTerminal / "jquery.terminal.js"   minified "jquery.terminal.min.js" dependsOn "jquery.js"
      ),
      // Build a js dependencies file
      skip in packageJSDependencies := false,
      // Put the jsdeps file on a place reachable for the server
      crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
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
    .dependsOn(edu_gemini_seqexec_web_shared_JS)

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

  // Project for the server side application
  lazy val edu_gemini_seqexec_web_server = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.server"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(ScalaZCore.value, UnboundId, JwtCore, StreamZ) ++ Http4s ++ Play,

      // Settings to optimize the use of sbt-revolver

      // Allows to read the generated JS on client
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.map((x: sbt.File) => new File(x.getAbsolutePath + ".map")).data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in(edu_gemini_seqexec_web_client, Compile, packageJSDependencies)).value,
      // Support stopping the running server
      //mainClass in reStart := Some("edu.gemini.seqexec.web.server.play.WebServerLauncher"),
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
    .dependsOn(edu_gemini_seqexec_web_shared_JVM, edu_gemini_seqexec_server)

}