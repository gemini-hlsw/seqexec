import Settings.Libraries._
import Settings.LibraryVersions
import Settings.Plugins
import Common._
import AppsCommon._
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport.crossProject
import sbt.Keys._

name := Settings.Definitions.name

organization in Global := "edu.gemini.ocs"

// sbt-header requires these settings even though we're using a custom license header
organizationName in ThisBuild := "Association of Universities for Research in Astronomy, Inc. (AURA)"
startYear        in ThisBuild := Some(2017)
licenses         in ThisBuild += ("BSD-3-Clause", new URL("https://opensource.org/licenses/BSD-3-Clause"))

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"

// This key is used to find the JRE dir. It could/should be overriden on a user basis
// Add e.g. a `jres.sbt` file with your particular configuration
ocsJreDir in ThisBuild := Path.userHome / ".jres8_ocs3"

parallelExecution in (ThisBuild, Test) := false

cancelable in Global := true

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  if (sys.props.contains("ocs3.skipDependencyUpdates")) s
  else "dependencyUpdates" :: s
}

// Uncomment for local gmp testing
//resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// Settings to use git to define the version of the project
def versionFmt(out: sbtdynver.GitDescribeOutput): String = {
  val dirtySuffix = if (out.dirtySuffix.mkString("", "").nonEmpty) {
    "-UNCOMMITED"
  } else {
    ""
  }
  s"-${out.commitSuffix.sha}$dirtySuffix"
}

def fallbackVersion(d: java.util.Date): String = s"HEAD-${sbtdynver.DynVer timestamp d}"

val dateFormatter = java.time.format.DateTimeFormatter.BASIC_ISO_DATE

inThisBuild(List(
  version := dateFormatter.format(dynverCurrentDate.value.toInstant.atZone(java.time.ZoneId.of("UTC")).toLocalDate) + dynverGitDescribeOutput.value.mkVersion(versionFmt, fallbackVersion(dynverCurrentDate.value))
))

enablePlugins(GitBranchPrompt)

val startAllCommands = List(
  "edu_gemini_seqexec_web_server/reStart",
  "edu_gemini_seqexec_web_client/fastOptJS::startWebpackDevServer",
  "~edu_gemini_seqexec_web_client/fastOptJS"
)
val restartWDSCommands = List(
  "edu_gemini_seqexec_web_client/fastOptJS::stopWebpackDevServer",
  "edu_gemini_seqexec_web_client/fastOptJS::startWebpackDevServer"
)

addCommandAlias("startAll", startAllCommands.mkString(";", ";", ""))

addCommandAlias("restartWDS", restartWDSCommands.mkString(";", ";", ""))

resolvers in ThisBuild +=
  Resolver.sonatypeRepo("snapshots")

//////////////
// Projects
//////////////
lazy val edu_gemini_giapi_client = project
  .in(file("modules/edu.gemini.giapi.client"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(Shapeless.value, CatsEffect.value, Fs2, GiapiJmsUtil, GiapiJmsProvider, GiapiStatusService, Giapi, GiapiCommandsClient) ++ Logging
  )

lazy val edu_gemini_web_server_common = project
  .in(file("modules/edu.gemini.web.server.common"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= CatsEffect.value +: (Http4s ++ Logging)
  )

lazy val edu_gemini_web_client_facades = project
  .in(file("modules/edu.gemini.web.client.facades"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonJSSettings: _*)
  .settings(
    scalacOptions ~= (_.filterNot(Set(
      // By necessity facades will have unused params
      "-Ywarn-unused:params"
    ))),
    libraryDependencies ++= Seq(ScalaJSDom.value, JQuery.value)
  )

// Root web project
lazy val edu_gemini_seqexec_web = project.in(file("modules/edu.gemini.seqexec.web"))
  .settings(commonSettings: _*)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .disablePlugins(RevolverPlugin)
  .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

// a special crossProject for configuring a JS/JVM/shared structure
lazy val edu_gemini_seqexec_web_shared = (crossProject.crossType(CrossType.Pure) in file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.shared"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .disablePlugins(RevolverPlugin)
  .dependsOn(edu_gemini_seqexec_model)
  .jvmSettings(commonSettings)
  .jsSettings(commonJSSettings)
  .jsSettings(
    libraryDependencies += JavaLogJS.value
  )

lazy val edu_gemini_seqexec_web_shared_JVM = edu_gemini_seqexec_web_shared.jvm

lazy val edu_gemini_seqexec_web_shared_JS = edu_gemini_seqexec_web_shared.js

// Project for the server side application
lazy val edu_gemini_seqexec_web_server = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.server"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(UnboundId, JwtCore, Knobs) ++ Http4s ++ Logging,
    // Supports launchin the server in the background
    mainClass in reStart := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
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

lazy val edu_gemini_seqexec_web_client = project.in(file("modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(TzdbPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .disablePlugins(RevolverPlugin)
  .settings(commonJSSettings: _*)
  .settings(
    wartremoverExcluded += sourceManaged.value / "main" / "java" / "time" / "zone" / "TzdbZoneRulesProvider.scala",
    zonesFilter := {(z: String) => z == "America/Santiago" || z == "Pacific/Honolulu"},
    // Needed for Monocle macros
    addCompilerPlugin(Plugins.paradisePlugin),
    webpackBundlingMode          := BundlingMode.LibraryOnly(),
    webpackMonitoredDirectories  += (resourceDirectory in Compile).value,
    // JS dependencies via npm
    npmDependencies in Compile ++= Seq(
      "react" -> LibraryVersions.reactJS,
      "react-dom" -> LibraryVersions.reactJS,
      "react-virtualized" -> LibraryVersions.reactVirtualized,
      "react-copy-to-clipboard" -> LibraryVersions.reactClipboard,
      "jquery" -> LibraryVersions.jQuery,
      "semantic-ui-dropdown" -> LibraryVersions.semanticUI,
      "semantic-ui-modal" -> LibraryVersions.semanticUI,
      "semantic-ui-progress" -> LibraryVersions.semanticUI,
      "semantic-ui-popup" -> LibraryVersions.semanticUI,
      "semantic-ui-tab" -> LibraryVersions.semanticUI,
      "semantic-ui-visibility" -> LibraryVersions.semanticUI,
      "semantic-ui-transition" -> LibraryVersions.semanticUI,
      "semantic-ui-dimmer" -> LibraryVersions.semanticUI,
      "semantic-ui-less" -> LibraryVersions.semanticUI
    ),
    npmDevDependencies in Compile ++= Seq(
      "postcss-loader" -> "2.0.8",
      "autoprefixer" -> "8.0.0",
      "url-loader" -> "1.0.1",
      "file-loader" -> "1.1.11",
      "css-loader" -> "0.28.11",
      "style-loader" -> "0.20.2",
      "less" -> "2.3.1",
      "less-loader" -> "4.1.0",
      "webpack-merge" -> "4.1.2",
      "webpack-dev-server-status-bar" -> "1.0.0",
      "cssnano" -> "3.10.0",
      "copy-webpack-plugin" -> "4.5.1",
      "clean-webpack-plugin" -> "0.1.19",
      "html-webpack-plugin" -> "3.0.6",
      "extract-text-webpack-plugin" -> "4.0.0-beta.0",
      "uglifyjs-webpack-plugin" -> LibraryVersions.uglifyJs,
      "cssnano" -> "3.10.0",
      "optimize-css-assets-webpack-plugin" -> "4.0.0"
    ),
    // Use a different Webpack configuration file for production and create a single bundle without source maps
    version in webpack               := "4.1.1",
    version in startWebpackDevServer := "3.1.1",
    webpackConfigFile in fullOptJS   := Some(baseDirectory.value / "prod.webpack.config.js"),
    webpackEmitSourceMaps            := false,
    webpackExtraArgs                 := Seq("--profile", "--progress", "true"),
    webpackDevServerExtraArgs        := Seq("--profile"),
    webpackConfigFile in fastOptJS   := Some(baseDirectory.value / "dev.webpack.config.js"),
    emitSourceMaps                   := false,
    // Requires the DOM for tests
    requiresDOM in Test              := true,
    // Use yarn as it is faster than npm
    useYarn                          := true,
    libraryDependencies             ++= Seq(
      JQuery.value,
      CatsEffect.value,
      ScalaCSS.value,
      ScalaJSDom.value,
      JavaTimeJS.value,
      JavaLogJS.value,
      ScalaJSReactVirtualized.value,
      ScalaJSReactClipboard.value
    ) ++ ReactScalaJS.value ++ Diode.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "edu.gemini.seqexec.web.client"
  )
  .dependsOn(edu_gemini_web_client_facades, edu_gemini_seqexec_web_shared_JS % "compile->compile;test->test", edu_gemini_seqexec_model_JS % "compile->compile;test->test")

// List all the modules and their inter dependencies
lazy val edu_gemini_seqexec_server = project
  .in(file("modules/edu.gemini.seqexec.server"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(edu_gemini_seqexec_engine, edu_gemini_seqexec_model_JVM, edu_gemini_epics_acm)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++=
      Seq(Argonaut,
          CommonsHttp,
          Squants.value,
          // OCS bundles
          SpModelCore,
          POT,
          Knobs,
          OpenCSV,
          Log4s,
          Http4sBoopickle
      ) ++ SeqexecOdb ++ WDBAClient ++ TestLibs.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "edu.gemini.seqexec.server"
  )

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val edu_gemini_seqexec_model = crossProject.crossType(CrossType.Pure)
  .in(file("modules/edu.gemini.seqexec.model"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= BooPickle.value +: Monocle.value
  )
  .jvmSettings(
    commonSettings)
  .jsSettings(commonJSSettings)
  .jsSettings(
    // And add a custom one
    libraryDependencies += JavaTimeJS.value
  )

lazy val edu_gemini_seqexec_model_JVM:Project = edu_gemini_seqexec_model.jvm

lazy val edu_gemini_seqexec_model_JS:Project = edu_gemini_seqexec_model.js

lazy val edu_gemini_seqexec_engine = project
  .in(file("modules/edu.gemini.seqexec.engine"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .dependsOn(edu_gemini_seqexec_model_JVM)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(Fs2, CatsEffect.value, Log4s) ++ Monocle.value
  )

lazy val edu_gemini_epics_acm = project
  .in(file("modules/edu.gemini.epics.acm"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      EpicsService,
      GmpCommandsRecords,
      Guava,
      Slf4j,
      XmlUnit,
      JUnitInterface
    ),
    sourceGenerators in Compile += Def.task {
      import scala.sys.process._
      val pkg = "edu.gemini.epics.acm.generated"
      val log = state.value.log
      val gen = (sourceManaged in Compile).value
      val out = (gen /: pkg.split("\\."))(_ / _)
      val xsd = sourceDirectory.value / "main" / "resources" / "CaSchema.xsd"
      val cmd = List("xjc",
        "-d", gen.getAbsolutePath,
        "-p", pkg,
        xsd.getAbsolutePath)
      val mod = xsd.getParentFile.listFiles.map(_.lastModified).max
      val cur = if (out.exists && out.listFiles.nonEmpty) out.listFiles.map(_.lastModified).min else Int.MaxValue
      if (mod > cur) {
        out.mkdirs
        val err = cmd.run(ProcessLogger(log.info(_), log.error(_))).exitValue
        if (err != 0) sys.error("xjc failed")
      }
      out.listFiles.toSeq
    }.taskValue
  )

/**
  * Common settings for the Seqexec instances
  */
lazy val seqexecCommonSettings = Seq(
  // Main class for launching
  mainClass in Compile := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
  // This is important to keep the file generation order correctly
  parallelExecution in Universal := false,
  // Run full opt js on the javascript. They will be placed on the "seqexec" jar
  resources in Compile ++= (webpack in (edu_gemini_seqexec_web_client, Compile, fullOptJS)).value.map(_.data),
  resources in Compile ++= ((resourceManaged in (edu_gemini_seqexec_web_client, Compile, fullOptJS)).value ** "*.mp3").get,
  resources in Compile ++= ((resourceManaged in (edu_gemini_seqexec_web_client, Compile, fullOptJS)).value ** "*.css").get,
  test := {},
  // Name of the launch script
  executableScriptName := "seqexec-server",
  // No javadocs
  mappings in (Compile, packageDoc) := Seq(),
  // Don't create launchers for Windows
  makeBatScripts := Seq.empty,
  // Specify a different name for the config file
  bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
  bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"""",
  // Copy logback.xml to let users customize it on site
  mappings in Universal += {
    val f = (resourceDirectory in (edu_gemini_seqexec_web_server, Compile)).value / "logback.xml"
    f -> ("conf/" + f.getName)
  },
  // Launch options
  javaOptions in Universal ++= Seq(
    // -J params will be added as jvm parameters
    "-J-Xmx1024m",
    "-J-Xms256m",
    // Support remote JMX access
    "-J-Dcom.sun.management.jmxremote",
    "-J-Dcom.sun.management.jmxremote.authenticate=false",
    "-J-Dcom.sun.management.jmxremote.port=2407",
    "-J-Dcom.sun.management.jmxremote.ssl=false",
    // Ensure the local is correctly set
    "-J-Duser.language=en",
    "-J-Duser.country=US",
    // Support remote debugging
    "-J-Xdebug",
    "-J-Xnoagent",
    "-J-verbose:gc",
    "-J-XX:+PrintGCDetails",
    "-J-XX:+PrintGCTimeStamps",
    "-J-XX:+HeapDumpOnOutOfMemoryError",
    // Make sure the application exits on OOM
    "-J-XX:+ExitOnOutOfMemoryError",
    "-J-XX:+CrashOnOutOfMemoryError",
    "-J-XX:HeapDumpPath=/tmp",
    "-J-Xrunjdwp:transport=dt_socket,address=8457,server=y,suspend=n"
  )
) ++ commonSettings

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
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(
    description := "Seqexec server for local testing",

    // Put the jar files in the lib dir
    mappings in Universal += {
      val jar = (packageBin in Compile).value
      jar -> ("lib/" + jar.getName)
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
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
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
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
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
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
  .settings(deployedAppMappings: _*)
  .settings(embeddedJreSettingsLinux64: _*)
  .settings(
    description := "Seqexec Gemini South server production",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GS
  ).dependsOn(seqexec_server)

/**
  * Project for the GN seqexec server app for production on Linux 64
  */
lazy val seqexec_server_gn = preventPublication(project.in(file("app/seqexec-server-gn")))
  .dependsOn(edu_gemini_seqexec_web_server)
  .aggregate(edu_gemini_seqexec_web_server)
  .enablePlugins(LinuxPlugin, RpmPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecRPMSettings: _*)
  .settings(deployedAppMappings: _*)
  .settings(embeddedJreSettingsLinux64: _*)
  .settings(
    description := "Seqexec Gemini North server production",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GN
  ).dependsOn(seqexec_server)
