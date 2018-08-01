import Settings.Libraries._
import Settings.LibraryVersions
import Settings.Plugins
import Common._
import AppsCommon._
import sbt.Keys._
import NativePackagerHelper._
import sbtcrossproject.{crossProject, CrossType}
import com.typesafe.sbt.packager.docker._

name := Settings.Definitions.name

organization in Global := "edu.gemini.ocs"

// sbt-header requires these settings even though we're using a custom license header
organizationName in ThisBuild := "Association of Universities for Research in Astronomy, Inc. (AURA)"
startYear        in ThisBuild := Some(2018)
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
resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

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

// Custom commands to facilitate web development
val startSeqexecAllCommands = List(
  "seqexec_web_server/reStart",
  "seqexec_web_client/fastOptJS::startWebpackDevServer",
  "~seqexec_web_client/fastOptJS"
)
val restartSeqexecWDSCommands = List(
  "seqexec_web_client/fastOptJS::stopWebpackDevServer",
  "seqexec_web_client/fastOptJS::startWebpackDevServer"
)
val stopSeqexecAllCommands = List(
  "seqexec_web_server/reStop",
  "seqexec_web_client/fastOptJS::stopWebpackDevServer"
)

addCommandAlias("startSeqexecAll", startSeqexecAllCommands.mkString(";", ";", ""))
addCommandAlias("restartSeqexecWDS", restartSeqexecWDSCommands.mkString(";", ";", ""))
addCommandAlias("stopSeqexecAll", stopSeqexecAllCommands.mkString(";", ";", ""))
addCommandAlias("genEnums", "; sql/runMain gem.sql.Main modules/core/shared/src/main/scala/gem/enum")
addCommandAlias("schemaSpy", "sql/runMain org.schemaspy.Main -t pgsql -port 5432 -db gem -o modules/sql/target/schemaspy -u postgres -host localhost -s public")
addCommandAlias("gemctl", "ctl/runMain gem.ctl.main")//

resolvers in ThisBuild +=
  Resolver.sonatypeRepo("snapshots")

// // Before printing the prompt check git to make sure all is well.
// shellPrompt in ThisBuild := { state =>
//   if (version.value != imageManifest.formatVersion) {
//     import scala.Console.{ RED, RESET }
//     print(RED)
//     println(s"Computed version doesn't match the filesystem anymore.")
//     println(s"Please `reload` to get back in sync.")
//     print(RESET)
//   }
//   "> "
// }

///////////////
// Root project
///////////////
lazy val ocs3 = preventPublication(project.in(file(".")))
  .settings(commonSettings)
  .aggregate(
    core.jvm,
    core.js,
    db,
    json.jvm,
    json.js,
    ocs2,
    ephemeris,
    service,
    telnetd,
    ctl,
    web,
    sql,
    main,
    ui,
    giapi,
    web_server_common,
    web_client_common,
    seqexec_model.js,
    seqexec_model.jvm,
    seqexec_engine,
    seqexec_server,
    seqexec_web_shared.js,
    seqexec_web_shared.jvm,
    seqexec_web_server,
    seqexec_web_client)

//////////////
// Projects
//////////////
lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(
      Cats.value,
      CatsEffect.value,
      Mouse.value,
      Shapeless.value,
      Atto.value
    ) ++ Monocle.value
  )
  .jsConfigure(
    _.enablePlugins(TzdbPlugin)
  ).jvmConfigure(
    _.enablePlugins(AutomateHeaderPlugin)
  ).jsSettings(
    libraryDependencies +=
      JavaTimeJS.value,
    wartremoverExcluded += sourceManaged.value / "main" / "java" / "time" / "zone" / "TzdbZoneRulesProvider.scala",
    // We only care about these two timezones. UTC is implicitly included
    zonesFilter         := {(z: String) => z == "America/Santiago" || z == "Pacific/Honolulu"}
  )
  .jsSettings(commonJSSettings)
  .jvmSettings(
    libraryDependencies += Fs2
  )

lazy val db = project
  .in(file("modules/db"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core.jvm % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Doobie,
    initialCommands += """
      |import cats._, cats.data._, cats.implicits._, cats.effect._
      |import doobie._, doobie.implicits._
      |import gem._, gem.enum._, gem.math._, gem.dao._, gem.dao.meta._, gem.dao.composite._
      |val xa = Transactor.fromDriverManager[IO](
      |  "org.postgresql.Driver",
      |  "jdbc:postgresql:gem",
      |  "postgres",
      |  "")
      |val y = xa.yolo
      |import y._
    """.stripMargin.trim
  )

lazy val json = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/json"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Circe.value
  )
  .jsSettings(commonJSSettings)
  .jvmConfigure(
    _.enablePlugins(AutomateHeaderPlugin)
  )

lazy val sql = project
  .in(file("modules/sql"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings ++ flywaySettings)
  .settings(
    libraryDependencies ++= Seq(
      Flyway
    ) ++ Doobie
  )

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core.jvm, db, sql)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      Fs2,
      ScalaXml.value,
      ScalaParserCombinators.value,
      Http4sXml,
    ) ++ Http4sClient ++ Http4s
  )

lazy val ephemeris = project
  .in(file("modules/ephemeris"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core.jvm % "compile->compile;test->test", db, sql)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      Fs2IO
    ) ++ Http4sClient
  )

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core.jvm, db, ephemeris, ocs2)
  .settings(commonSettings)

lazy val telnetd = project
  .in(file("modules/telnetd"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Tuco
  )

lazy val web = project
  .in(file("modules/web"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql, json.jvm)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      Slf4j,
      Http4sCirce,
      JwtCore
    ) ++ Http4s
  )

lazy val ui = project
  .in(file("modules/ui"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js, json.js)
  .settings(commonSettings)
  .settings(commonJSSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true
  )

lazy val ctl = project
  .in(file("modules/ctl"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings (
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    resolvers += Resolver.bintrayRepo("bkirwi", "maven"),
    libraryDependencies ++= Seq(
      CatsEffect.value,
      CatsFree.value,
      Decline
    ),
    TaskKey[Unit]("deployTest") := Def.taskDyn {
      (runMain in Compile).toTask {
        s" gem.ctl.main --verbose --host sbfocstest-lv1.cl.gemini.edu deploy-test ${version.value}"
      }
    } .value,
    fork in run := true
  )

lazy val imageManifest = SettingKey[ImageManifest]("imageManifest")

lazy val main = project
  .in(file("modules/main"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(web, telnetd)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(
    imageManifest         := ImageManifest.current("postgres:9.6.0", version.value).unsafeRunSync,
    packageName in Docker := "gem",
    dockerBaseImage       := "openjdk:8u141",
    dockerExposedPorts    := List(9090, 9091),
    dockerRepository      := Some("sbfocsdev-lv1.cl.gemini.edu"),
    dockerLabels          := imageManifest.value.labels,

    // Install nc before changing the user
    dockerCommands       ++= dockerCommands.value.flatMap {
      case c @ Cmd("USER", args @ _*) =>
        Seq(
          ExecCmd("RUN", "apt-get", "update"),
          ExecCmd("RUN", "apt-get", "--assume-yes", "install", "netcat-openbsd"),
          c
        )
      case cmd => Seq(cmd)
    },

    // Generate a file containing our git history
    (mappings in Universal) += {
      val out  = (target in Compile).value / "GIT_HISTORY"
      val data = imageManifest.value.history
      IO.writeLines(out, data.toList)
      (out, "GIT_HISTORY")
    }

  )

lazy val giapi = project
  .in(file("modules/giapi"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(Cats.value, Mouse.value, Shapeless.value, CatsEffect.value, Fs2, GiapiJmsUtil, GiapiJmsProvider, GiapiStatusService, Giapi, GiapiCommandsClient) ++ Logging,
    libraryDependencies ++= Seq(GmpStatusGateway % "test", GmpStatusDatabase % "test", GmpCmdJmsBridge % "test", NopSlf4j % "test"),
    excludeDependencies ++= Seq(
      // Remove to silence logging on tests
      ExclusionRule("ch.qos.logback", "logback-classic")
    )
  )

// Common utilities for web server projects
lazy val web_server_common = project
  .in(file("modules/shared/web/server/"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= CatsEffect.value +: (Http4s ++ Logging)
  )

// Common utilities for web client projects
lazy val web_client_common = project
  .in(file("modules/shared/web/client"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonJSSettings: _*)
  .settings(
    scalacOptions ~= (_.filterNot(Set(
      // By necessity facades will have unused params
      "-Ywarn-unused:params"
    ))),
    // Needed for Monocle macros
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(
      Cats.value,
      Mouse.value,
      ScalaJSDom.value,
      JQuery.value,
      ScalaJSReactVirtualized.value,
      ScalaJSReactDraggable.value) ++ ReactScalaJS.value ++ Monocle.value
  )

// a special crossProject for configuring a JS/JVM/shared structure
lazy val seqexec_web_shared = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/seqexec/web/shared"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .disablePlugins(RevolverPlugin)
  .jvmSettings(commonSettings)
  .jsSettings(commonJSSettings)
  .jsSettings(
    libraryDependencies += JavaLogJS.value,
  )
  .dependsOn(seqexec_model % "compile->compile;test->test")

// Project for the server side application
lazy val seqexec_web_server = project.in(file("modules/seqexec/web/server"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(UnboundId, JwtCore, Knobs, Http4sPrometheus) ++ Http4sClient ++ Http4s ++ Logging,
    // Supports launching the server in the background
    mainClass in reStart := Some("seqexec.web.server.http4s.WebServerLauncher"),
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoKeys += buildInfoBuildNumber,
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "seqexec.web.server"
  )
  .dependsOn(seqexec_web_shared.jvm, seqexec_server, web_server_common)

lazy val seqexec_web_client = project.in(file("modules/seqexec/web/client"))
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
    // Configurations for webpack
    webpackBundlingMode in fastOptJS         := BundlingMode.LibraryOnly(),
    webpackBundlingMode in fullOptJS         := BundlingMode.Application,
    webpackResources                         := (baseDirectory.value / "src" / "webpack") * "*.js",
    webpackDevServerPort                     := 9090,
    version in webpack                       := "4.12.0",
    version in startWebpackDevServer         := "3.1.4",
    // Use a different Webpack configuration file for production and create a single bundle without source maps
    webpackConfigFile in fullOptJS           := Some(baseDirectory.value / "src" / "webpack" / "prod.webpack.config.js"),
    webpackConfigFile in fastOptJS           := Some(baseDirectory.value / "src" / "webpack" / "dev.webpack.config.js"),
    webpackConfigFile in Test                := Some(baseDirectory.value / "src" / "webpack" / "test.webpack.config.js"),
    webpackEmitSourceMaps                    := false,
    webpackExtraArgs                         := Seq("--progress", "true"),
    emitSourceMaps                           := false,
    parallelExecution in Test                := false,
    // Requires the DOM for tests
    requiresDOM in Test                      := true,
    // Use yarn as it is faster than npm
    useYarn                                  := true,
    // JS dependencies via npm
    npmDependencies in Compile ++= Seq(
      "react"                   -> LibraryVersions.reactJS,
      "react-dom"               -> LibraryVersions.reactJS,
      "react-virtualized"       -> LibraryVersions.reactVirtualized,
      "react-draggable"         -> LibraryVersions.reactDraggable,
      "jquery"                  -> LibraryVersions.jQuery,
      "semantic-ui-dropdown"    -> LibraryVersions.semanticUI,
      "semantic-ui-modal"       -> LibraryVersions.semanticUI,
      "semantic-ui-progress"    -> LibraryVersions.semanticUI,
      "semantic-ui-popup"       -> LibraryVersions.semanticUI,
      "semantic-ui-tab"         -> LibraryVersions.semanticUI,
      "semantic-ui-visibility"  -> LibraryVersions.semanticUI,
      "semantic-ui-transition"  -> LibraryVersions.semanticUI,
      "semantic-ui-dimmer"      -> LibraryVersions.semanticUI,
      "semantic-ui-less"        -> LibraryVersions.semanticUI
    ),
    // NPM libs for development, mostly to let webpack do its magic
    npmDevDependencies in Compile ++= Seq(
      "postcss-loader"                     -> "2.1.5",
      "autoprefixer"                       -> "8.6.3",
      "url-loader"                         -> "1.0.1",
      "file-loader"                        -> "1.1.11",
      "css-loader"                         -> "0.28.11",
      "style-loader"                       -> "0.21.0",
      "less"                               -> "2.3.1",
      "less-loader"                        -> "4.1.0",
      "webpack-merge"                      -> "4.1.3",
      "mini-css-extract-plugin"            -> "0.4.0",
      "webpack-dev-server-status-bar"      -> "1.0.0",
      "cssnano"                            -> "3.10.0",
      "uglifyjs-webpack-plugin"            -> "1.2.5",
      "html-webpack-plugin"                -> "3.2.0",
      "optimize-css-assets-webpack-plugin" -> "4.0.2",
      "favicons-webpack-plugin"            -> "0.0.9"
    ),
    libraryDependencies ++= Seq(
      JQuery.value,
      Cats.value,
      Mouse.value,
      CatsEffect.value,
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
    buildInfoPackage := "seqexec.web.client"
  )
  .dependsOn(web_client_common, seqexec_web_shared.js % "compile->compile;test->test", seqexec_model.js % "compile->compile;test->test")

// List all the modules and their inter dependencies
lazy val seqexec_server = project
  .in(file("modules/seqexec/server"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.paradisePlugin),
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
          Http4sXml,
          Http4sBoopickle
      ) ++ Http4s ++ Http4sClient ++ SeqexecOdb ++ Monocle.value ++ WDBAClient ++ TestLibs.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "seqexec.server"
  )
  .dependsOn(seqexec_engine, giapi, seqexec_model.jvm % "compile->compile;test->test", acm, core.jvm % "test->test")

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val seqexec_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/seqexec/model"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(Mouse.value, BooPickle.value) ++ Monocle.value
  )
  .jvmSettings(
    commonSettings)
  .jsSettings(commonJSSettings)
  .jsSettings(
    // And add a custom one
    libraryDependencies += JavaTimeJS.value
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val seqexec_engine = project
  .in(file("modules/seqexec/engine"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(GitBranchPrompt)
  .dependsOn(seqexec_model.jvm)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(Fs2, CatsEffect.value, Log4s) ++ Monocle.value
  )

lazy val acm = project
  .in(file("modules/acm"))
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
  mainClass in Compile := Some("seqexec.web.server.http4s.WebServerLauncher"),
  // This is important to keep the file generation order correctly
  parallelExecution in Universal := false,
  // Depend on webpack and add the assets created by webpack
  mappings in (Compile, packageBin) ++= (webpack in (seqexec_web_client, Compile, fullOptJS)).value.map { f => f.data -> f.data.getName() },
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
    val f = (resourceDirectory in (seqexec_web_server, Compile)).value / "logback.xml"
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
    // These are very verbose but could be useful when tracking momeroy leaks
    // "-J-XX:+PrintGCDetails",
    // "-J-XX:+PrintGCTimeStamps",
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
lazy val app_seqexec_server = preventPublication(project.in(file("app/seqexec-server")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
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
lazy val app_seqexec_server_gs_test = preventPublication(project.in(file("app/seqexec-server-gs-test")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
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
lazy val app_seqexec_server_gn_test = preventPublication(project.in(file("app/seqexec-server-gn-test")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
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
lazy val app_seqexec_server_gs = preventPublication(project.in(file("app/seqexec-server-gs")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
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
lazy val app_seqexec_server_gn = preventPublication(project.in(file("app/seqexec-server-gn")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
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
