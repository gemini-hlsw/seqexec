import sbt._
import java.lang.{ Runtime => JRuntime }
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

/**
  * Application settings and dependencies
  */
object Settings {
  object Definitions {

    /** The name of the application */
    val name = "ocs3"

    //"-Ybackend-parallelism", JRuntime.getRuntime.availableProcessors.toString, // Run some tasks in parallel
  }

  /** Library versions */
  object LibraryVersions {
    val scalaVersion = "2.12.9"

    // ScalaJS libraries
    val scalaDom                = "0.9.7"
    val scalajsReact            = "1.4.2"
    val booPickle               = "1.3.1"
    val diode                   = "1.1.5"
    val diodeReact              = "1.1.5.142"
    val javaTimeJS              = "2.0.0-RC3"
    val scalaJQuery             = "1.2"
    val scalaJSReactCommon      = "0.2.5"
    val scalaJSReactVirtualized = "0.6.5"
    val scalaJSReactClipboard   = "0.8.0"
    val scalaJSReactDraggable   = "0.4.3"
    val scalaJSReactSortable    = "0.2.1"
    val reactSemanticUI         = "0.1.4"

    // Scala libraries
    val catsEffectVersion       = "2.0.0"
    val catsVersion             = "2.0.0"
    val mouseVersion            = "0.23"
    val fs2Version              = "2.0.1"
    val shapelessVersion        = "2.3.3"
    val attoVersion             = "0.7.0"
    val scalaParsersVersion     = "1.1.2"
    val scalaXmlVerson          = "1.2.0"

    val http4sVersion           = "0.20.10"
    val squants                 = "1.5.0"
    val argonaut                = "6.2.3"
    val commonsHttp             = "2.0.2"
    val unboundId               = "3.2.1"
    val jwt                     = "2.1.0"
    val slf4j                   = "1.7.26"
    val log4s                   = "1.8.2"
    val log4cats                = "0.2.0"
    val logback                 = "1.2.3"
    val janino                  = "3.1.0"
    val logstash                = "6.2"
    val knobs                   = "7.0.24"
    val monocleVersion          = "2.0.0"
    val circeVersion            = "0.12.0"
    val doobieVersion           = "0.6.0"
    val flywayVersion           = "6.0.1"
    val tucoVersion             = "0.4.1"
    val declineVersion          = "0.6.2"

    // test libraries
    val scalaTest               = "3.0.5"
    val scalaCheck              = "1.13.5"
    val discipline              = "0.9.0"
    val xmlUnit                 = "1.6"
    val jUnitInterface          = "0.11"
    val scalaMock               = "4.1.0"

    // Pure JS libraries
    val jQuery                  = "3.2.1"
    val semanticUI              = "2.3.1"
    val ocsVersion              = "2019101.1.4"
    val uglifyJs                = "1.2.4"

    val apacheXMLRPC            = "3.1.3"
    val opencsv                 = "2.1"
    val epicsService            = "1.0.2"
    val gmpCommandRecords       = "0.7.2"
    val giapi                   = "1.1.2"
    val giapiJmsUtil            = "0.5.2"
    val giapiJmsProvider        = "1.6.2"
    val giapiCommandsClient     = "0.2.2"
    val giapiStatusService      = "0.6.2"
    val gmpStatusGateway        = "0.3.2"
    val gmpStatusDatabase       = "0.3.2"
    val gmpCmdClientBridge      = "0.6.2"
    val guava                   = "25.0-jre"
    val prometheusClient        = "0.6.0"
    val geminiLocales           = "0.1.0-2019a"

    // Gemini Libraries
    val gspMath                 = "0.1.6"
  }

  /**
    * Global libraries
    */
  object Libraries {
    // Test Libraries
    val TestLibs               = Def.setting(Seq(
      "edu.gemini"              %%% "gsp-math-testkit"              % LibraryVersions.gspMath         % "test",
    ))
    val XmlUnit                = "xmlunit" % "xmlunit" % LibraryVersions.xmlUnit % "test"
    val JUnitInterface         = "com.novocode" % "junit-interface" % LibraryVersions.jUnitInterface % "test"
    val ScalaMock              = "org.scalamock" %% "scalamock" % LibraryVersions.scalaMock % "test"
    // Server side libraries
    val Cats                   = Def.setting("org.typelevel" %%% "cats-core"                         % LibraryVersions.catsVersion)
    val CatsEffect             = Def.setting("org.typelevel" %%% "cats-effect"                       % LibraryVersions.catsEffectVersion)
    val CatsFree               = Def.setting("org.typelevel" %%% "cats-free"                         % LibraryVersions.catsVersion)
    val Fs2                    = "co.fs2"                    %%  "fs2-core"                          % LibraryVersions.fs2Version
    val Fs2IO                  = "co.fs2"                    %%  "fs2-io"                            % LibraryVersions.fs2Version % "test"
    val Mouse                  = Def.setting("org.typelevel" %%% "mouse"                             % LibraryVersions.mouseVersion)
    val Shapeless              = Def.setting("com.chuusai"   %%% "shapeless"                         % LibraryVersions.shapelessVersion)
    val Decline                = "com.monovore"              %%  "decline"                           % LibraryVersions.declineVersion
    val Argonaut               = "io.argonaut"               %%  "argonaut"                          % LibraryVersions.argonaut
    val CommonsHttp            = "commons-httpclient"        %   "commons-httpclient"                % LibraryVersions.commonsHttp
    val UnboundId              = "com.unboundid"             %   "unboundid-ldapsdk-minimal-edition" % LibraryVersions.unboundId
    val JwtCore                = "com.pauldijou"             %%  "jwt-core"                          % LibraryVersions.jwt
    val Slf4j                  = "org.slf4j"                 %   "slf4j-api"                         % LibraryVersions.slf4j
    val JuliSlf4j              = "org.slf4j"                 %   "jul-to-slf4j"                      % LibraryVersions.slf4j
    val NopSlf4j               = "org.slf4j"                 %   "slf4j-nop"                         % LibraryVersions.slf4j
    val Log4Cats               = Def.setting("io.chrisdavenport" %%% "log4cats-slf4j" % LibraryVersions.log4cats)
    val Log4CatsNoop           = Def.setting("io.chrisdavenport" %%% "log4cats-noop" % LibraryVersions.log4cats % "test")
    val Logback                = Seq(
      "ch.qos.logback"       % "logback-core"             % LibraryVersions.logback,
      "ch.qos.logback"       % "logback-classic"          % LibraryVersions.logback,
      "org.codehaus.janino"  % "janino"                   % LibraryVersions.janino,
      "net.logstash.logback" % "logstash-logback-encoder" % LibraryVersions.logstash
    )
    val Log4s                  = Def.setting("org.log4s"              %%% "log4s"                    % LibraryVersions.log4s)
    val PrometheusClient       = "io.prometheus"                      %   "simpleclient_common"      % LibraryVersions.prometheusClient
    val Logging                = Def.setting(Seq(JuliSlf4j, Log4s.value) ++ Logback)
    val Knobs                  = "io.getnelson.knobs"                 %%  "core"                     % LibraryVersions.knobs
    val OpenCSV                = "net.sf.opencsv"                     %   "opencsv"                  % LibraryVersions.opencsv
    val Squants                = Def.setting("org.typelevel"          %%% "squants"                  % LibraryVersions.squants)
    val ScalaXml               = Def.setting("org.scala-lang.modules" %%% "scala-xml"                % LibraryVersions.scalaXmlVerson)
    val ScalaParserCombinators = Def.setting("org.scala-lang.modules" %%  "scala-parser-combinators" % LibraryVersions.scalaParsersVersion)
    val Http4s                 = Seq(
      "org.http4s" %% "http4s-dsl"          % LibraryVersions.http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % LibraryVersions.http4sVersion)
    val Http4sClient           = Seq(
      "org.http4s" %% "http4s-dsl"          % LibraryVersions.http4sVersion,
      "org.http4s" %% "http4s-async-http-client" % LibraryVersions.http4sVersion)
    val Http4sBoopickle        = "org.http4s"                         %%  "http4s-boopickle"                 % LibraryVersions.http4sVersion
    val Http4sCirce            = "org.http4s"                         %%  "http4s-circe"                     % LibraryVersions.http4sVersion
    val Http4sXml              = "org.http4s"                         %%  "http4s-scala-xml"                 % LibraryVersions.http4sVersion
    val Http4sPrometheus       = "org.http4s"                         %%  "http4s-prometheus-metrics" % LibraryVersions.http4sVersion
    val Flyway                 = "org.flywaydb"                       %   "flyway-core"                      % LibraryVersions.flywayVersion
    val Atto                   = Def.setting("org.tpolecat"           %%% "atto-core"                        % LibraryVersions.attoVersion)
    val Monocle                = Def.setting(Seq(
      "com.github.julien-truffaut" %%% "monocle-core"   % LibraryVersions.monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"  % LibraryVersions.monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-unsafe" % LibraryVersions.monocleVersion,
    ))
    val Tuco                   = Seq(
      "org.tpolecat" %% "tuco-core"  % LibraryVersions.tucoVersion,
      "org.tpolecat" %% "tuco-shell" % LibraryVersions.tucoVersion
    )
    val Circe                  = Def.setting(Seq(
      "io.circe" %%% "circe-core"    % LibraryVersions.circeVersion,
      "io.circe" %%% "circe-generic" % LibraryVersions.circeVersion,
      "io.circe" %%% "circe-parser"  % LibraryVersions.circeVersion,
      "io.circe" %%% "circe-testing" % LibraryVersions.circeVersion % "test"))
    val Doobie                 = Seq(
      "org.tpolecat" %% "doobie-core"      % LibraryVersions.doobieVersion,
      "org.tpolecat" %% "doobie-postgres"  % LibraryVersions.doobieVersion,
      "org.tpolecat" %% "doobie-scalatest" % LibraryVersions.doobieVersion % "test")

    // Client Side JS libraries
    val ReactScalaJS            = Def.setting(Seq(
      "com.github.japgolly.scalajs-react" %%% "core"             % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalajs-react" %%% "extra"            % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalajs-react" %%% "ext-monocle-cats" % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalajs-react" %%% "ext-cats"         % LibraryVersions.scalajsReact
    ))
    val Diode                   = Def.setting(Seq(
      "io.suzaku" %%% "diode"       % LibraryVersions.diode,
      "io.suzaku" %%% "diode-react" % LibraryVersions.diodeReact
    ))
    val ScalaJSDom              = Def.setting("org.scala-js"            %%% "scalajs-dom"        % LibraryVersions.scalaDom)
    val ScalaJSReactCommon      = Def.setting("io.github.cquiroz.react" %%% "common"             % LibraryVersions.scalaJSReactCommon)
    val ScalaJSReactCats        = Def.setting("io.github.cquiroz.react" %%% "cats"               % LibraryVersions.scalaJSReactCommon)
    val ScalaJSReactVirtualized = Def.setting("io.github.cquiroz.react" %%% "react-virtualized"  % LibraryVersions.scalaJSReactVirtualized)
    val ScalaJSReactDraggable   = Def.setting("io.github.cquiroz.react" %%% "react-draggable"    % LibraryVersions.scalaJSReactDraggable)
    val ScalaJSReactSortable    = Def.setting("io.github.cquiroz.react" %%% "react-sortable-hoc" % LibraryVersions.scalaJSReactSortable)
    val ScalaJSReactClipboard   = Def.setting("io.github.cquiroz.react" %%% "react-clipboard"    % LibraryVersions.scalaJSReactClipboard)
    val JQuery                  = Def.setting("org.querki"              %%% "jquery-facade"      % LibraryVersions.scalaJQuery)
    val BooPickle               = Def.setting("io.suzaku"               %%% "boopickle"          % LibraryVersions.booPickle)
    val JavaTimeJS              = Def.setting("io.github.cquiroz"       %%% "scala-java-time"    % LibraryVersions.javaTimeJS)
    val GeminiLocales           = Def.setting("edu.gemini"              %%% "gemini-locales"     % LibraryVersions.geminiLocales)

    // OCS Libraries, these should become modules in the future
    val SpModelCore = "edu.gemini.ocs"    %% "edu-gemini-spmodel-core"        % LibraryVersions.ocsVersion
    val SeqexecOdb  = Seq(
                      "edu.gemini.ocs"    %% "edu-gemini-seqexec-odb"         % LibraryVersions.ocsVersion,
                      "dom4j"             %  "dom4j"                          % "1.5.1"
                        exclude("jaxen", "jaxen")
                        exclude("jaxme", "jaxme-api")
                        exclude("msv", "xsdlib")
                        exclude("msv", "relaxngDatatype")
                        exclude("pull-parser", "pull-parser")
                        exclude("stax", "stax")
                        exclude("xml-apis", "xml-apis")
                        exclude("xpp3", "xpp3"))
    val POT         = "edu.gemini.ocs"    %% "edu-gemini-pot"                 % LibraryVersions.ocsVersion
    val TRPC        = "edu.gemini.ocs"    %% "edu-gemini-util-trpc"           % LibraryVersions.ocsVersion
    val WDBAClient  = Seq(
                      "edu.gemini.ocs"    %% "edu-gemini-wdba-session-client" % LibraryVersions.ocsVersion,
                      "org.apache.xmlrpc" %  "xmlrpc-client"                  % LibraryVersions.apacheXMLRPC)

    // GIAPI Libraries
    val EpicsService        = "edu.gemini.epics"     % "epics-service"           % LibraryVersions.epicsService
    val GmpCommandsRecords  = "edu.gemini.gmp"       % "gmp-commands-records"    % LibraryVersions.gmpCommandRecords
    val GiapiJmsUtil        = "edu.gemini.aspen"     % "giapi-jms-util"          % LibraryVersions.giapiJmsUtil
    val GiapiJmsProvider    = "edu.gemini.jms"       % "jms-activemq-provider"   % LibraryVersions.giapiJmsProvider
    val Giapi               = "edu.gemini.aspen"     % "giapi"                   % LibraryVersions.giapi
    val GiapiCommandsClient = "edu.gemini.aspen.gmp" % "gmp-commands-jms-client" % LibraryVersions.giapiCommandsClient
    val GiapiStatusService  = "edu.gemini.aspen"     % "giapi-status-service"    % LibraryVersions.giapiStatusService
    val GmpStatusGateway    = "edu.gemini.aspen.gmp" % "gmp-status-gateway"      % LibraryVersions.gmpStatusGateway
    val GmpStatusDatabase   = "edu.gemini.aspen.gmp" % "gmp-statusdb"            % LibraryVersions.gmpStatusDatabase
    val GmpCmdJmsBridge     = "edu.gemini.aspen.gmp" % "gmp-commands-jms-bridge" % LibraryVersions.gmpCmdClientBridge
    val Guava               = "com.google.guava"     % "guava"                   % LibraryVersions.guava

    // Gemini Libraries
    val GspMath             = Def.setting("edu.gemini" %%% "gsp-math"         % LibraryVersions.gspMath         )
    val GspMathTestkit      = Def.setting("edu.gemini" %%% "gsp-math-testkit" % LibraryVersions.gspMath % "test")
  }

  object PluginVersions {
    // Compiler plugins
    val paradiseVersion  = "2.1.1"
    val kpVersion        = "0.9.10"
    val betterMonadicFor = "0.3.1"
  }

  object Plugins {
    val paradisePlugin = ("org.scalamacros" %% "paradise" % PluginVersions.paradiseVersion) .cross(CrossVersion.patch)
    val kindProjectorPlugin    = "org.spire-math" %% "kind-projector" % PluginVersions.kpVersion
    val betterMonadicForPlugin = "com.olegpy" %% "better-monadic-for" % PluginVersions.betterMonadicFor
  }

}
