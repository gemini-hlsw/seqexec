import Settings.Libraries._
import Settings.LibraryVersions
import Common._
import AppsCommon._
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport.crossProject
import sbt.Keys._

name := Settings.Definitions.name

scalaVersion in ThisBuild := Settings.LibraryVersions.scala

scalacOptions in ThisBuild ++= Settings.Definitions.scalacOptions

organization in Global := "edu.gemini.ocs"

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"

// This key is used to find the JRE dir. It could/should be overriden on a user basis
// Add e.g. a `jres.sbt` file with your particular configuration
ocsJreDir in ThisBuild := Path.userHome / ".jres8"

lazy val edu_gemini_web_server_common = project
  .in(file("modules/edu.gemini.web.server.common"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(ScalaZConcurrent) ++ Http4s
  )


// Root web project
lazy val edu_gemini_seqexec_web = project.in(file("modules/edu.gemini.seqexec.web"))
  .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

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
    reStart := (reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client, Compile))).evaluated,
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
    reStart := (reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client_cli, Compile))).evaluated
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoKeys += buildInfoBuildNumber,
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "edu.gemini.seqexec.web.server"
  )
  .dependsOn(edu_gemini_seqexec_web_shared_JVM, edu_gemini_seqexec_server, edu_gemini_web_server_common)

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
    (scalaJSNativeLibraries in Test) := (scalaJSNativeLibraries in Test).map { l =>
      l.map(virtualFiles => virtualFiles.filter(vf => {
        val f = vf.toURI.toString
        !(f.endsWith(".js") && f.contains("scala/tools"))
      }))
    }.value,
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

// List all the modules and their inter dependencies
lazy val edu_gemini_seqexec_server = project
  .in(file("modules/edu.gemini.seqexec.server"))
  .dependsOn(edu_gemini_seqexec_engine, edu_gemini_seqexec_model_JVM)
  .settings(
    libraryDependencies ++=
      Seq(ScalaZStream,
          Argonaut,
          CommonsHttp,
          Squants.value,
          // OCS bundles
          SpModelCore,
          SeqexecOdb,
          POT,
          EpicsACM,
          Knobs
      ) ++ WDBAClient ++ TestLibs.value
  )

// This should eventually replaced by seqexec_server
lazy val edu_gemini_seqexec_engine = project
  .in(file("modules/edu.gemini.seqexec.engine"))
  .dependsOn(edu_gemini_seqexec_model_JVM)
  .settings(libraryDependencies ++= Seq(ScalaZStream) ++ TestLibs.value)

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val edu_gemini_seqexec_model = crossProject.crossType(CrossType.Pure)
  .in(file("modules/edu.gemini.seqexec.model"))
  .settings(libraryDependencies ++= Seq(BooPickle.value, ScalaZCore.value) ++ TestLibs.value)

lazy val edu_gemini_seqexec_model_JVM:Project = edu_gemini_seqexec_model.jvm

lazy val edu_gemini_seqexec_model_JS:Project = edu_gemini_seqexec_model.js

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
  test := {},
  // Name of the launch script
  executableScriptName := "seqexec-server",
  // No javadocs
  mappings in (Compile, packageDoc) := Seq(),
  // Don't create launchers for Windows
  makeBatScripts := Seq.empty,
  // Specify a different name for the config file
  bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
  // Launch options
  javaOptions in Universal ++= Seq(
    // -J params will be added as jvm parameters
    "-J-Xmx512m",
    "-J-Xms256m"
  )
)

/**
  * Settings for Seqexec RPMs
  */
lazy val seqexecRPMSettings = Seq(
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
  }
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
    mappings in Universal += {
      val jar = (packageBin in Compile).value
      jar -> ("lib/" + jar.getName)
    }
  )

lazy val seqexecTestServerSettings = Seq(
  // Put the jre on the RPM
  linuxPackageMappings in Rpm += {
    val jresDir = (ocsJreDir in ThisBuild).value
    // RPM are of interest only for linux 64 bit
    val linux64Jre = jresDir.toPath.resolve("linux").resolve("JRE64_1.8")
    packageDirectoryAndContentsMapping((linux64Jre.toFile, (rpmPrefix in Rpm).value.map(_ + "").getOrElse("")))
  }
)

/**
  * Project for the seqexec test server at GS on Linux 64
  */
lazy val seqexec_server_gs_test = preventPublication(project.in(file("app/seqexec-server-gs-test")))
  .dependsOn(edu_gemini_seqexec_web_server)
  .aggregate(edu_gemini_seqexec_web_server)
  .enablePlugins(LinuxPlugin, RpmPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
  .settings(seqexecTestServerSettings: _*)
  .settings(deployedAppMappings: _*)
  .settings(embeddedJreSettingsLinux64: _*)
  .settings(
    description := "Seqexec GS test deployment"
  ).dependsOn(seqexec_server)

/**
  * Project for the seqexec test server at GN on Linux 64
  */
lazy val seqexec_server_gn_test = preventPublication(project.in(file("app/seqexec-server-gn-test")))
  .dependsOn(edu_gemini_seqexec_web_server)
  .aggregate(edu_gemini_seqexec_web_server)
  .enablePlugins(LinuxPlugin, RpmPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
  .settings(seqexecTestServerSettings: _*)
  .settings(deployedAppMappings: _*)
  .settings(embeddedJreSettingsLinux64: _*)
  .settings(
    description := "Seqexec GN test deployment"
  ).dependsOn(seqexec_server)

/**
  * Project for the seqexec server app for production on Linux 64
  */
lazy val seqexec_server_gs = preventPublication(project.in(file("app/seqexec-server-gs")))
  .dependsOn(edu_gemini_seqexec_web_server)
  .aggregate(edu_gemini_seqexec_web_server)
  .enablePlugins(LinuxPlugin, RpmPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
  .settings(deployedAppMappings: _*)
  .settings(embeddedJreSettingsLinux64: _*)
  .settings(configurationFromSVN: _*)
  .settings(
    description := "Seqexec Gemini South server production",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GS,

    // Download the configuration from svn
    packageZipTarball in Universal := ((packageZipTarball in Universal).dependsOn(downloadConfiguration)).value,
    packageBin in Rpm := ((packageBin in Rpm).dependsOn(downloadConfiguration)).value,

    // Put the jre on the RPM
    linuxPackageMappings in Rpm += {
      val jresDir = (ocsJreDir in ThisBuild).value
      // RPM are of interest only for linux 64 bit
      val linux64Jre = jresDir.toPath.resolve("linux").resolve("JRE64_1.8")
      packageDirectoryAndContentsMapping((linux64Jre.toFile, (rpmPrefix in Rpm).value.map(_ + "").getOrElse("")))
    }
  ).dependsOn(seqexec_server)

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
    reStart := (reStart dependsOn (fastOptJS in (edu_gemini_p1backend_client, Compile))).evaluated,
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
  .dependsOn(edu_gemini_p1backend_shared_JVM, edu_gemini_web_server_common)

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
    (scalaJSNativeLibraries in Test) := (scalaJSNativeLibraries in Test).map { l =>
      l.map(virtualFiles => virtualFiles.filter(vf => {
        val f = vf.toURI.toString
        !(f.endsWith(".js") && f.contains("scala/tools"))
      }))
    }.value,
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

