import Settings.Libraries._
import Settings.LibraryVersions
import Settings.Plugins
import Common._
import AppsCommon._
import sbt.Keys._
import NativePackagerHelper._
import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import com.typesafe.sbt.packager.docker._

name := Settings.Definitions.name

organization in Global := "edu.gemini.ocs"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / publishArtifact in (Compile, packageDoc) := false

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"

// This key is used to find the JRE dir. It could/should be overriden on a user basis
// Add e.g. a `jres.sbt` file with your particular configuration
ocsJreDir in ThisBuild := Path.userHome / ".jres8_ocs3"

parallelExecution in (ThisBuild, Test) := false

cancelable in Global := true

// Should make CI builds more robust
concurrentRestrictions in Global += Tags.limit(ScalaJSTags.Link, 2)

// Uncomment for local gmp testing
// resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

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
  "seqexec_web_client/fastOptJS::startWebpackDevServer",
  "~seqexec_web_client/fastOptJS"
)
val stopSeqexecAllCommands = List(
  "seqexec_web_server/reStop",
  "seqexec_web_client/fastOptJS::stopWebpackDevServer"
)

addCommandAlias("startSeqexecAll", startSeqexecAllCommands.mkString(";", ";", ""))
addCommandAlias("restartSeqexecWDS", restartSeqexecWDSCommands.mkString(";", ";", ""))
addCommandAlias("stopSeqexecAll", stopSeqexecAllCommands.mkString(";", ";", ""))
addCommandAlias("genEnums", "; sql/runMain gem.sql.Main modules/core/shared/src/main/scala/gem/enum")
addCommandAlias("rebuildEnums", "; sql/flywayClean; sql/flywayMigrate; genEnums; coreJVM/compile")
addCommandAlias("schemaSpy", "sql/runMain org.schemaspy.Main -t pgsql -port 5432 -db gem -o modules/sql/target/schemaspy -u postgres -host localhost -s public")

resolvers in ThisBuild +=
  Resolver.sonatypeRepo("public")

resolvers in ThisBuild +=
  Resolver.sonatypeRepo("snapshots")

updateOptions in ThisBuild := updateOptions.value.withLatestSnapshots(false)

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
    ocs2_api.jvm,
    ocs2_api.js,
    ocs2,
    ephemeris,
    sql,
    giapi,
    web_server_common,
    web_client_common,
    seqexec_model.js,
    seqexec_model.jvm,
    seqexec_engine,
    seqexec_server,
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
      Atto.value,
      GspMath.value,
    ) ++ Monocle.value ++ TestLibs.value
  ).jsSettings(
    libraryDependencies ++=
      Seq(JavaTimeJS.value, GeminiLocales.value)
  )
  .jsSettings(gspScalaJsSettings)
  .jvmSettings(
    libraryDependencies += Fs2
  )

lazy val db = project
  .in(file("modules/db"))
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
  .dependsOn(core % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Circe.value
  )
  .jsSettings(gspScalaJsSettings)

lazy val sql = project
  .in(file("modules/sql"))
  .settings(commonSettings ++ flywaySettings)
  .settings(
    libraryDependencies ++= Seq(
      Flyway
    ) ++ Doobie
  )

lazy val ocs2_api = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/ocs2_api"))
  .dependsOn(core)
  .settings(commonSettings)
  .jsSettings(gspScalaJsSettings)

lazy val ocs2_api_JVM = ocs2_api.jvm

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .dependsOn(core.jvm, db, sql, ocs2_api_JVM)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      Fs2,
      ScalaXml.value,
      Http4sXml
    ) ++ Http4sClient ++ Http4s
  )

lazy val ephemeris = project
  .in(file("modules/ephemeris"))
  .dependsOn(core.jvm % "compile->compile;test->test", db, sql)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      Fs2IO
    ) ++ Http4sClient
  )

lazy val giapi = project
  .in(file("modules/giapi"))
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(Cats.value, Mouse.value, Shapeless.value, CatsEffect.value, Fs2, GiapiJmsUtil, GiapiJmsProvider, GiapiStatusService, Giapi, GiapiCommandsClient) ++ Logging.value ++ Monocle.value,
    libraryDependencies ++= Seq(GmpStatusGateway % "test", GmpStatusDatabase % "test", GmpCmdJmsBridge % "test", NopSlf4j % "test")
  )

// Common utilities for web server projects
lazy val web_server_common = project
  .in(file("modules/shared/web/server/"))
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= CatsEffect.value +: (Http4s ++ Logging.value)
  )

// Common utilities for web client projects
lazy val web_client_common = project
  .in(file("modules/shared/web/client"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(gspScalaJsSettings: _*)
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
      ScalaJSReactCommon.value,
      ScalaJSReactCats.value,
      ScalaJSReactVirtualized.value,
      ScalaJSReactSortable.value,
      ScalaJSReactDraggable.value) ++ ReactScalaJS.value ++ Monocle.value ++ TestLibs.value
  )

// Project for the server side application
lazy val seqexec_web_server = project.in(file("modules/seqexec/web/server"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    libraryDependencies ++= Seq(
      UnboundId,
      JwtCore,
      JwtCirce,
      Http4sPrometheus,
      CommonsHttp,
      ScalaMock,
      Log4CatsNoop.value) ++
      Http4sClient ++ Http4s ++ PureConfig ++ Logging.value ,
    // Supports launching the server in the background
    javaOptions in reStart += s"-javaagent:${(baseDirectory in ThisBuild).value}/app/seqexec-server/src/universal/bin/jmx_prometheus_javaagent-0.3.1.jar=6060:${(baseDirectory in ThisBuild).value}/app/seqexec-server/src/universal/bin/prometheus.yaml",
    mainClass in reStart := Some("seqexec.web.server.http4s.WebServerLauncher")
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := BuildInfoKey.ofN(name, version, buildInfoBuildNumber),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "seqexec.web.server"
  )
  .dependsOn(seqexec_server, web_server_common, core.jvm % "compile->compile;test->test")
  .dependsOn(seqexec_model.jvm % "compile->compile;test->test")

lazy val seqexec_web_client = project.in(file("modules/seqexec/web/client"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(GitBranchPrompt)
  .disablePlugins(RevolverPlugin)
  .settings(gspScalaJsSettings: _*)
  .settings(
    // Needed for Monocle macros
    addCompilerPlugin(Plugins.paradisePlugin),
    // Configurations for webpack
    webpackBundlingMode in fastOptJS         := BundlingMode.LibraryOnly(),
    webpackBundlingMode in fullOptJS         := BundlingMode.Application,
    webpackResources                         := (baseDirectory.value / "src" / "webpack") * "*.js",
    webpackDevServerPort                     := 9090,
    version in webpack                       := "4.41.2",
    version in startWebpackDevServer         := "3.9.0",
    // Use a different Webpack configuration file for production and create a single bundle without source maps
    webpackConfigFile in fullOptJS           := Some(baseDirectory.value / "src" / "webpack" / "prod.webpack.config.js"),
    webpackConfigFile in fastOptJS           := Some(baseDirectory.value / "src" / "webpack" / "dev.webpack.config.js"),
    webpackConfigFile in Test                := Some(baseDirectory.value / "src" / "webpack" / "test.webpack.config.js"),
    webpackEmitSourceMaps                    := false,
    emitSourceMaps                           := false,
    parallelExecution in Test                := false,
    version in installJsdom                  := "12.0.0",
    requireJsDomEnv in Test                  := true,
    // Use yarn as it is faster than npm
    useYarn                                  := true,
    // JS dependencies via npm
    npmDependencies in Compile ++= Seq(
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
      "postcss-loader"                     -> "3.0.0",
      "autoprefixer"                       -> "9.7.1",
      "url-loader"                         -> "2.2.0",
      "file-loader"                        -> "4.2.0",
      "css-loader"                         -> "3.2.0",
      "style-loader"                       -> "1.0.0",
      "less"                               -> "2.7.2",
      "less-loader"                        -> "4.1.0",
      "webpack-merge"                      -> "4.2.2",
      "mini-css-extract-plugin"            -> "0.8.0",
      "webpack-dev-server-status-bar"      -> "1.1.0",
      "cssnano"                            -> "4.1.10",
      "uglifyjs-webpack-plugin"            -> "2.2.0",
      "html-webpack-plugin"                -> "3.2.0",
      "optimize-css-assets-webpack-plugin" -> "5.0.3",
      "favicons-webpack-plugin"            -> "1.0.2",
      "why-did-you-update"                 -> "1.0.6",
      "@packtracker/webpack-plugin"        -> "2.2.0"
    ),
    libraryDependencies ++= Seq(
      JQuery.value,
      Cats.value,
      Mouse.value,
      CatsEffect.value,
      ScalaJSDom.value,
      JavaTimeJS.value,
      Log4s.value,
      ScalaJSReactVirtualized.value,
      ScalaJSReactClipboard.value,
      GeminiLocales.value,
      PPrint.value
    ) ++ ReactScalaJS.value ++ Diode.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := BuildInfoKey.ofN(name, version),
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "seqexec.web.client"
  )
  .dependsOn(
    web_client_common % "compile->compile;test->test",
    seqexec_model.js % "compile->compile;test->test")

// List all the modules and their inter dependencies
lazy val seqexec_server = project
  .in(file("modules/seqexec/server"))
  .enablePlugins(GitBranchPrompt)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.paradisePlugin),
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    addCompilerPlugin(Plugins.betterMonadicForPlugin),
    libraryDependencies ++=
      Seq(Http4sCirce,
          Squants.value,
          // OCS bundles
          SpModelCore,
          POT,
          OpenCSV,
          Http4sXml,
          Http4sBoopickle,
          PrometheusClient,
          Log4Cats.value,
          Log4CatsNoop.value
      ) ++ Http4s ++ Http4sClient ++ PureConfig ++ SeqexecOdb ++ Monocle.value ++ WDBAClient ++ TestLibs.value ++
        Circe.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := BuildInfoKey.ofN(name, version),
    buildInfoObject := "OcsBuildInfo",
    buildInfoPackage := "seqexec.server"
  )
  .dependsOn(seqexec_engine, ocs2_api.jvm, giapi, seqexec_model.jvm % "compile->compile;test->test", acm, core.jvm % "test->test")

// Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
// We have to define the project properties at this level
lazy val seqexec_model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/seqexec/model"))
  .enablePlugins(GitBranchPrompt)
  .settings(
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(
      Squants.value,
      Mouse.value,
      BooPickle.value) ++ Monocle.value,
    Test / libraryDependencies += GspMathTestkit.value
  )
  .jvmSettings(
    commonSettings,
    libraryDependencies += Http4sCore
  )
  .jsSettings(gspScalaJsSettings)
  .jsSettings(
    // And add a custom one
    libraryDependencies += JavaTimeJS.value
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val seqexec_engine = project
  .in(file("modules/seqexec/engine"))
  .enablePlugins(GitBranchPrompt)
  .dependsOn(seqexec_model.jvm % "compile->compile;test->test")
  .settings(commonSettings: _*)
  .settings(
    addCompilerPlugin(Plugins.kindProjectorPlugin),
    addCompilerPlugin(Plugins.paradisePlugin),
    libraryDependencies ++= Seq(Fs2, CatsEffect.value, Log4s.value, Log4Cats.value) ++ Monocle.value
  )

lazy val acm = project
  .in(file("modules/acm"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      EpicsService,
      GmpCommandsRecords,
      Guava,
      Slf4j,
      XmlUnit,
      ScalaMock,
      JUnitInterface
    ) ++ Logback,
    libraryDependencies in Test ++= Logback,
    testOptions in Test := Seq(),
    sourceGenerators in Compile += Def.task {
      import scala.sys.process._
      val pkg = "edu.gemini.epics.acm.generated"
      val log = state.value.log
      val gen = (sourceManaged in Compile).value
      val out = pkg.split("\\.").foldLeft(gen)(_ / _)
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
  bashScriptExtraDefines += """addJava "-javaagent:${app_home}/jmx_prometheus_javaagent-0.3.1.jar=6060:${app_home}/prometheus.yaml"""",
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
    "-J-XX:+HeapDumpOnOutOfMemoryError",
    // Make sure the application exits on OOM
    "-J-XX:+ExitOnOutOfMemoryError",
    "-J-XX:+CrashOnOutOfMemoryError",
    "-J-XX:HeapDumpPath=/tmp",
    "-J-Xrunjdwp:transport=dt_socket,address=8457,server=y,suspend=n",
    "-java-home ${app_home}/../jre" // This breaks builds without jre
  )
) ++ commonSettings

/**
  * Settings for Seqexec in Linux
  */
lazy val seqexecLinux = Seq(
  // User/Group for execution
  daemonUser in Linux := "software",
  daemonGroup in Linux := "software",
  maintainer in Universal := "Software Group <software@gemini.edu>",
  // This lets us build RPMs from snapshot versions
  name in Linux := "Seqexec Server",
  version in Linux := {
    (version in ThisBuild).value.replace("-SNAPSHOT", "").replace("-", "_").replace(" ", "")
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
    },
    mappings in Universal := {
      // filter out sjs jar files. otherwise it could generate some conflicts
      val universalMappings = (mappings in Universal).value
      val filtered = universalMappings filter {
        case (_, name) => !name.contains("_sjs")
      }
      filtered
    },
    mappings in Universal += {
      val f = (resourceDirectory in Compile).value / "update_smartgcal"
      f -> ("bin/" + f.getName)
    },
    mappings in Universal += {
      val f = (resourceDirectory in Compile).value / "seqexec-server.env"
      f -> ("systemd/" + f.getName)
    },
    mappings in Universal += {
      val f = (resourceDirectory in Compile).value / "seqexec-server.service"
      f -> ("systemd/" + f.getName)
    }
  )

/**
  * Project for the seqexec test server at GS on Linux 64
  */
lazy val app_seqexec_server_gs_test = preventPublication(project.in(file("app/seqexec-server-gs-test")))
  .dependsOn(seqexec_web_server, seqexec_web_client)
  .aggregate(seqexec_web_server, seqexec_web_client)
  .enablePlugins(LinuxPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .enablePlugins(SystemdPlugin)
  .enablePlugins(GitBranchPrompt)
  .settings(seqexecCommonSettings: _*)
  .settings(seqexecLinux: _*)
  .settings(deployedAppMappings: _*)
  .settings(
    description := "Seqexec GS test deployment",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GS,
    mappings in Universal := {
      // filter out sjs jar files. otherwise it could generate some conflicts
      val universalMappings = (mappings in (app_seqexec_server, Universal)).value
      val filtered = universalMappings filter {
        case (_, name) => !name.contains("_sjs")
      }
      filtered
    }
  )
  .settings(embeddedJreSettingsLinux64: _*)
  .dependsOn(seqexec_server)

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
  .settings(seqexecLinux: _*)
  .settings(deployedAppMappings: _*)
  .settings(
    description := "Seqexec GN test deployment",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GN,
    mappings in Universal := {
      // filter out sjs jar files. otherwise it could generate some conflicts
      val universalMappings = (mappings in (app_seqexec_server, Universal)).value
      val filtered = universalMappings filter {
        case (_, name) => !name.contains("_sjs")
      }
      filtered
    }
  )
  .settings(embeddedJreSettingsLinux64: _*)
  .dependsOn(seqexec_server)

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
  .settings(seqexecLinux: _*)
  .settings(deployedAppMappings: _*)
  .settings(
    description := "Seqexec Gemini South server production",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GS,
    mappings in Universal := {
      // filter out sjs jar files. otherwise it could generate some conflicts
      val universalMappings = (mappings in (app_seqexec_server, Universal)).value
      val filtered = universalMappings filter {
        case (_, name) => !name.contains("_sjs")
      }
      filtered
    }
  )
  .settings(embeddedJreSettingsLinux64: _*)
  .dependsOn(seqexec_server)

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
  .settings(seqexecLinux: _*)
  .settings(deployedAppMappings: _*)
  .settings(
    description := "Seqexec Gemini North server production",
    applicationConfName := "seqexec",
    applicationConfSite := DeploymentSite.GN,
    mappings in Universal := {
      // filter out sjs jar files. otherwise it could generate some conflicts
      val universalMappings = (mappings in (app_seqexec_server, Universal)).value
      val filtered = universalMappings filter {
        case (_, name) => !name.contains("_sjs")
      }
      filtered
    }
  )
  .settings(embeddedJreSettingsLinux64: _*)
  .dependsOn(seqexec_server)
