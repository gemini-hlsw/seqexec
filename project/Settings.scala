import sbt._
import java.lang.{Runtime => JRuntime}
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

/**
 * Application settings and dependencies
 */
object Settings {
  object Definitions {
    /** The name of the application */
    val name = "ocs3"

    /** Options for the scala compiler */
    val scalacOptions = Seq(
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-encoding", "utf-8",                // Specify character encoding used by source files.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
      "-language:higherKinds",             // Allow higher-kinded types
      "-language:implicitConversions",     // Allow definition of implicit functions called views
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
      "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xfuture",                          // Turn on future language features.
      "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
      "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
      "-Xlint:option-implicit",            // Option.apply used implicit view.
      "-Xlint:package-object-classes",     // Class or object defined in package object.
      "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match",              // Pattern match may not be typesafe.
      "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification",             // Enable partial unification in type constructor inference
      "-Ywarn-dead-code",                  // Warn when dead code is identified.
      "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen",              // Warn when numerics are widened.
      "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals",              // Warn if a local definition is unused.
      "-Ywarn-unused:params",              // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates",            // Warn if a private member is unused.
      "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
      "-Ybackend-parallelism", JRuntime.getRuntime.availableProcessors.toString // Run some tasks in parallel
    )
  }

  /** Library versions */
  object LibraryVersions {
    val scalaVersion = "2.12.6"

    // ScalaJS libraries
    val scalaDom                = "0.9.6"
    val scalajsReact            = "1.2.3"
    val booPickle               = "1.3.0"
    val diode                   = "1.1.3"
    val diodeReact              = "1.1.3.120"
    val javaTimeJS              = "2.0.0-M13"
    val javaLogJS               = "0.1.5"
    val scalaJQuery             = "1.2"
    val scalaJSReactVirtualized = "0.3.4"
    val scalaJSReactClipboard   = "0.4.0"
    val scalaJSReactDraggable   = "0.1.1"

    // Scala libraries
    val catsEffectVersion       = "0.10.1"
    val catsVersion             = "1.2.0"
    val mouseVersion            = "0.18"
    val fs2Version              = "0.10.5"
    val shapelessVersion        = "2.3.3"
    val attoVersion             = "0.6.3"
    val scalaParsersVersion     = "1.1.1"
    val scalaXmlVerson          = "1.1.0"

    val http4sVersion           = "0.18.15"
    val squants                 = "1.3.0"
    val argonaut                = "6.2.2"
    val commonsHttp             = "2.0.2"
    val unboundId               = "3.2.1"
    val jwt                     = "0.17.0"
    val slf4j                   = "1.7.25"
    val log4s                   = "1.6.1"
    val logback                 = "1.2.3"
    val janino                  = "3.0.8"
    val logstash                = "5.2"
    val knobs                   = "6.0.33"
    val monocleVersion          = "1.5.1-cats"
    val circeVersion            = "0.9.3"
    val doobieVersion           = "0.5.3"
    val flywayVersion           = "5.1.4"
    val tucoVersion             = "0.3.1"
    val declineVersion          = "0.4.2"

    // test libraries
    val scalaTest               = "3.0.5"
    val scalaCheck              = "1.13.5"
    val discipline              = "0.9.0"
    val xmlUnit                 = "1.6"
    val jUnitInterface          = "0.11"

    // Pure JS libraries
    val reactJS                 = "16.2.0"
    val jQuery                  = "3.2.1"
    val semanticUI              = "2.2.10"
    val ocsVersion              = "2018101.1.3"
    val uglifyJs                = "1.2.4"
    val reactVirtualized        = "9.20.1"
    val reactDraggable          = "3.0.5"

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
    val prometheusClient        = "0.5.0"
  }

  /**
    * Global libraries
    */
  object Libraries {
    // Test Libraries
    val TestLibs               = Def.setting(Seq(
      "org.typelevel"              %%% "cats-testkit"              % LibraryVersions.catsVersion         % "test"
    ))
    val XmlUnit                = "xmlunit" % "xmlunit" % LibraryVersions.xmlUnit % "test"
    val JUnitInterface         = "com.novocode" % "junit-interface" % LibraryVersions.jUnitInterface % "test"

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
    val Logback                = Seq(
      "ch.qos.logback"       % "logback-core"             % LibraryVersions.logback,
      "ch.qos.logback"       % "logback-classic"          % LibraryVersions.logback,
      "org.codehaus.janino"  % "janino"                   % LibraryVersions.janino,
      "net.logstash.logback" % "logstash-logback-encoder" % LibraryVersions.logstash
    )
    val Log4s                  = "org.log4s"                          %%  "log4s"                    % LibraryVersions.log4s
    val PrometheusClient       = "io.prometheus"                      %   "simpleclient_common"      % LibraryVersions.prometheusClient
    val Logging                = Seq(JuliSlf4j, Log4s) ++ Logback
    val Knobs                  = "io.verizon.knobs"                   %%  "core"                     % LibraryVersions.knobs
    val OpenCSV                = "net.sf.opencsv"                     %   "opencsv"                  % LibraryVersions.opencsv
    val Squants                = Def.setting("org.typelevel"          %%% "squants"                  % LibraryVersions.squants)
    val ScalaXml               = Def.setting("org.scala-lang.modules" %%% "scala-xml"                % LibraryVersions.scalaXmlVerson)
    val ScalaParserCombinators = Def.setting("org.scala-lang.modules" %%  "scala-parser-combinators" % LibraryVersions.scalaParsersVersion)
    val Http4s                 = Seq(
      "org.http4s" %% "http4s-dsl"          % LibraryVersions.http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % LibraryVersions.http4sVersion)
    val Http4sClient           = Seq(
      "org.http4s" %% "http4s-dsl"          % LibraryVersions.http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % LibraryVersions.http4sVersion)
    val Http4sBoopickle        = "org.http4s"                         %%  "http4s-boopickle"                 % LibraryVersions.http4sVersion
    val Http4sCirce            = "org.http4s"                         %%  "http4s-circe"                     % LibraryVersions.http4sVersion
    val Http4sXml              = "org.http4s"                         %%  "http4s-scala-xml"                 % LibraryVersions.http4sVersion
    val Http4sPrometheus       = "org.http4s"                         %%  "http4s-prometheus-server-metrics" % LibraryVersions.http4sVersion
    val Flyway                 = "org.flywaydb"                       %   "flyway-core"                      % LibraryVersions.flywayVersion
    val Atto                   = Def.setting("org.tpolecat"           %%% "atto-core"                        % LibraryVersions.attoVersion)
    val Monocle                = Def.setting(Seq(
      "com.github.julien-truffaut" %%% "monocle-core"   % LibraryVersions.monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"  % LibraryVersions.monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-unsafe" % LibraryVersions.monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-law"    % LibraryVersions.monocleVersion % "test"))
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
    val ScalaJSDom              = Def.setting("org.scala-js"      %%% "scalajs-dom"               % LibraryVersions.scalaDom)
    val ScalaJSReactVirtualized = Def.setting("io.github.cquiroz" %%% "scalajs-react-virtualized" % LibraryVersions.scalaJSReactVirtualized)
    val ScalaJSReactDraggable   = Def.setting("io.github.cquiroz" %%% "scalajs-react-draggable"   % LibraryVersions.scalaJSReactDraggable)
    val ScalaJSReactClipboard   = Def.setting("io.github.cquiroz" %%% "scalajs-react-clipboard"   % LibraryVersions.scalaJSReactClipboard)
    val JQuery                  = Def.setting("org.querki"        %%% "jquery-facade"             % LibraryVersions.scalaJQuery)
    val BooPickle               = Def.setting("io.suzaku"         %%% "boopickle"                 % LibraryVersions.booPickle)
    val JavaTimeJS              = Def.setting("io.github.cquiroz" %%% "scala-java-time"           % LibraryVersions.javaTimeJS)
    val JavaLogJS               = Def.setting("org.scala-js"      %%% "scalajs-java-logging"      % LibraryVersions.javaLogJS)

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
  }

  object PluginVersions {
    // Compiler plugins
    val paradiseVersion    = "2.1.1"
    val kpVersion          = "0.9.7"
  }

  object Plugins {
    val paradisePlugin = "org.scalamacros" %% "paradise" % PluginVersions.paradiseVersion cross CrossVersion.patch
    val kindProjectorPlugin = "org.spire-math" %% "kind-projector" % PluginVersions.kpVersion
  }

}
