import sbt.Keys._
import sbt._

import Settings.Libraries._
import Settings.LibraryVersions

import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin

import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaServerAppPackaging
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._
import com.typesafe.sbt.packager.linux.LinuxPlugin
import com.typesafe.sbt.packager.linux.LinuxPlugin.autoImport._
import com.typesafe.sbt.packager.rpm.RpmPlugin
import com.typesafe.sbt.packager.rpm.RpmPlugin.autoImport._
import NativePackagerHelper._

import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._

import spray.revolver.RevolverPlugin.autoImport._

object OcsBuild extends Build {

  lazy val ocsJreDir = settingKey[File]("Directory where distribution JREs are stored.")

  sealed trait LogType
  object LogType {
    case object ConsoleAndFiles extends LogType
    case object Files extends LogType
  }

  /**
    * Task to generate a logging configuration file from a template
    */
  def generateLoggingConfigTask(logType: LogType) = Def.task {
    val loggingConfDestName = "logging.properties"
    val loggingConfSrcName = logType match {
      case LogType.Files           => "logging.files.template"
      case LogType.ConsoleAndFiles => "logging.console_files.template"
    }

    val loggingTemplate = (baseDirectory in ThisBuild).value / "project" / "resources" / loggingConfSrcName
    val config = IO.read(loggingTemplate).replace("{{app.name}}", name.value)
    target.value.mkdirs()
    val destFile = target.value / loggingConfDestName
    println(s"Generating configuration of type $logType to ${destFile.getPath}")
    IO.write(destFile, config)
    destFile
  }

  /**
    * Settings for meta projects to make them non-publishable
    */
  def preventPublication(p: Project) =
    p.settings(
      publish := {},
      publishLocal := {},
      publishArtifact := false,
      publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      packagedArtifacts := Map.empty)

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
      scalaJSUseRhino := false,
      libraryDependencies += JavaLogJS.value
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
      artifactPath in (Compile, fullOptJS) := (resourceManaged in Compile).value / "seqexec-opt.js",
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
      artifactPath in (Compile, fullOptJS) := (resourceManaged in Compile).value / "seqexec-cli-opt.js",
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
      libraryDependencies ++= Seq(ScalaZCore.value, UnboundId, JwtCore, StreamZ, Slf4jJuli) ++ Http4s ++ Play,

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

  /**
    * Common settings for the Seqexec instances
    */
  lazy val seqexecCommonSettings = Seq(
    // Main class for launching
    mainClass in Compile := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
    // This is important to keep the file generation order correctly
    parallelExecution in Universal := false,
    // Run full opt js on the javascript. They will be placed on the "seqexec" jar
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client, Compile)).value,
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client_cli, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client_cli, Compile)).value,
    // Name of the launch script
    executableScriptName := "seqexec-server",
    // No javadocs
    mappings in (Compile, packageDoc) := Seq(),
    // Don't create launchers for Windows
    makeBatScript := None,
    // Specify a different name for the config file
    bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
    // Launch options
    javaOptions in Universal ++= Seq(
      // -J params will be added as jvm parameters
      "-J-Xmx512m",
      "-J-Xms256m",

      // app parameters
      // TODO Define how to configure applications
      "prod" // Run in production mode.
    )
  )

  /**
    * Project for the seqexec server app for development
    */
  lazy val seqexec_server = preventPublication(project.in(file("app/seqexec-server")))
    .dependsOn(edu_gemini_seqexec_web_server)
    .aggregate(edu_gemini_seqexec_web_server)
    .enablePlugins(JavaServerAppPackaging)
    .settings(seqexecCommonSettings: _*)
    .settings(
      description := "Seqexec server for local testing",

      // Generate a custom logging.properties for the application
      // For staging the log uses files and console
      mappings in Universal += {
        val f = generateLoggingConfigTask(LogType.ConsoleAndFiles).value
        f -> ("conf/" + f.getName)
      },

      // Put the jar files in the lib dir
      mappings in Universal <+= (packageBin in Compile) map { jar =>
        jar -> ("lib/" + jar.getName)
      }
    )

  /**
    * Project for the seqexec server app for testing on Linux 64
    */
  lazy val seqexec_server_test_l64 = preventPublication(project.in(file("app/seqexec-server-test-l64")))
    .enablePlugins(LinuxPlugin, RpmPlugin)
    .enablePlugins(JavaServerAppPackaging)
    .settings(seqexecCommonSettings: _*)
    .settings(
      description := "Seqexec server test deployment on linux 64",

      // RPM properties
      rpmVendor := "Gemini",
      rpmLicense := Some("BSD-3"),
      rpmGroup := Some("Gemini"),
      rpmChangelogFile := None,
      packageDescription in Rpm := "Seqexec Server",
      rpmPrefix in Rpm := Some("/gemsoft/opt"),
      packageName in Rpm := "seqexec-server",
      // User/Group for execution
      daemonUser in Linux := "telops",
      daemonGroup in Linux := "telops",
      // This lets us build RPMs from snapshot versions
      version in Rpm := {
        (version in ThisBuild).value.replace("-SNAPSHOT", "")
      },

      // The distribution uses only log files, no console
      mappings in Universal in packageZipTarball += {
        val f = generateLoggingConfigTask(LogType.Files).value
        f -> ("conf/" + f.getName)
      },

      // Put the jre in the tarball
      mappings in Universal ++= {
        val jresDir = (ocsJreDir in ThisBuild).value
        // Map the location of jre files
        val jreLink = "JRE64_1.8"
        val linux64Jre = jresDir.toPath.resolve("linux").resolve(jreLink)
        directory(linux64Jre.toFile).map { j =>
          j._1 -> j._2.replace(jreLink, "jre")
        }
      },

      // Make the launcher use the embedded jre
      javaOptions in Universal ++= Seq(
        "-java-home ${app_home}/../jre"
      ),

      // Put the jre on the RPM
      linuxPackageMappings in Rpm += {
        val jresDir = (ocsJreDir in ThisBuild).value
        // RPM are of interest only for linux 64 bit
        val linux64Jre = jresDir.toPath.resolve("linux").resolve("JRE64_1.8")
        packageDirectoryAndContentsMapping((linux64Jre.toFile, (rpmPrefix in Rpm).value.map(_ + "").getOrElse("")))
      }
    ).dependsOn(seqexec_server)

}