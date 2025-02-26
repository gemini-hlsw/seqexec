import sbt._
import java.lang.{ Runtime => JRuntime }
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

/**
 * Application settings and dependencies
 */
object Settings {

  /** Library versions */
  object LibraryVersions {
    // ScalaJS libraries
    val scalaDom                = "2.8.0"
    val scalajsReact            = "2.1.1"
    val booPickle               = "1.4.0"
    val diode                   = "1.2.0-RC4"
    val javaTimeJS              = "2.6.0"
    val scalaJSReactCommon      = "0.17.0"
    val scalaJSSemanticUI       = "0.16.0"
    val scalaJSReactVirtualized = "0.13.1"
    val scalaJSReactClipboard   = "1.5.1"
    val scalaJSReactDraggable   = "0.16.0"
    val scalaJSReactSortable    = "0.5.2"

    // Scala libraries
    val catsEffectVersion   = "3.5.7"
    val catsVersion         = "2.13.0"
    val mouseVersion        = "1.3.2"
    val fs2Version          = "3.10.2"
    val shapelessVersion    = "2.3.9"
    val scalaParsersVersion = "1.1.2"
    val scalaXmlVersion     = "1.2.0"
    val catsTime            = "0.5.1"

    val http4sVersion              = "0.23.30"
    val http4sBlazeVersion         = "0.23.17"
    val http4sJdkHttpClientVersion = "0.9.2"
    val http4sBoopickleVersion     = "0.23.11"
    val http4sScalaXmlVersion      = "0.23.14"

    val squants        = "1.8.3"
    val commonsHttp    = "2.0.2"
    val unboundId      = "3.2.1"
    val jwt            = "10.0.1"
    val slf4j          = "2.0.13"
    val log4s          = "1.10.0"
    val log4cats       = "2.7.0"
    val log4catsLevel  = "0.3.1"
    val logback        = "1.4.11"
    val janino         = "3.1.12"
    val logstash       = "7.0"
    val pureConfig     = "0.17.8"
    val monocleVersion = "3.3.0"
    val circeVersion   = "0.14.10"
    val doobieVersion  = "0.6.0"
    val flywayVersion  = "6.0.4"
    val scoptVersion   = "4.1.0"

    // test libraries
    val xmlUnit                = "1.6"
    val jUnitInterface         = "0.13.2"
    val scalaMock              = "6.2.0"
    val munitVersion           = "1.0.3"
    val munitDisciplineVersion = "2.0.0"
    val munitCatsEffectVersion = "2.0.0-M5"

    // Pure JS libraries
    val fomanticUI = "2.8.7"
    val ocsVersion = "2024101.1.3"

    val apacheXMLRPC        = "3.1.3"
    val opencsv             = "2.3"
    val epicsService        = "1.0.7"
    val gmpCommandRecords   = "0.7.7"
    val acm                 = "0.1.1"
    val giapi               = "1.1.7"
    val giapiScala          = "0.3-38643d9-SNAPSHOT"
    val giapiJmsUtil        = "0.5.7"
    val giapiJmsProvider    = "1.6.7"
    val giapiCommandsClient = "0.2.7"
    val giapiStatusService  = "0.6.7"
    val gmpStatusGateway    = "0.3.7"
    val gmpStatusDatabase   = "0.3.7"
    val gmpCmdClientBridge  = "0.6.7"
    val guava               = "31.0.1-jre"
    val geminiLocales       = "0.7.0"
    val pprint              = "0.9.0"
    val jaxb                = "2.3.1"

    // Gemini Libraries
    val lucumaCore    = "0.47.0"
    val lucumaUI      = "0.39.0"
    val lucumaSchemas = "0.33.0"

    val sttp = "3.7.4"
  }

  /**
   * Global libraries
   */
  object Libraries {
    // Test Libraries
    val TestLibs       = Def.setting(
      "org.typelevel" %%% "cats-testkit-scalatest" % "2.1.5" % "test"
    )
    val MUnit          = Def.setting(
      Seq(
        "org.scalameta" %%% "munit"             % LibraryVersions.munitVersion           % Test,
        "org.typelevel" %%% "munit-cats-effect" % LibraryVersions.munitCatsEffectVersion % Test,
        "org.typelevel" %%% "discipline-munit"  % LibraryVersions.munitDisciplineVersion % Test
      )
    )
    val XmlUnit        = "xmlunit" % "xmlunit" % LibraryVersions.xmlUnit % "test"
    val JUnitInterface =
      "com.github.sbt" % "junit-interface" % LibraryVersions.jUnitInterface % "test"
    val ScalaMock   = "org.scalamock"     %% "scalamock"          % LibraryVersions.scalaMock  % "test"
    // Server side libraries
    val Cats        = Def.setting("org.typelevel" %%% "cats-core" % LibraryVersions.catsVersion)
    val CatsEffect  =
      Def.setting("org.typelevel" %%% "cats-effect" % LibraryVersions.catsEffectVersion)
    val Fs2         = "co.fs2"            %% "fs2-core"           % LibraryVersions.fs2Version
    val Fs2IO       = "co.fs2"            %% "fs2-io"             % LibraryVersions.fs2Version % "test"
    val Mouse       = Def.setting("org.typelevel" %%% "mouse" % LibraryVersions.mouseVersion)
    val Scopt       = Def.setting("com.github.scopt" %% "scopt" % LibraryVersions.scoptVersion)
    val Shapeless   = Def.setting("com.chuusai" %%% "shapeless" % LibraryVersions.shapelessVersion)
    val CommonsHttp = "commons-httpclient" % "commons-httpclient" % LibraryVersions.commonsHttp
    val UnboundId   =
      "com.unboundid" % "unboundid-ldapsdk-minimal-edition" % LibraryVersions.unboundId
    val JwtCore          = "com.github.jwt-scala" %% "jwt-core"     % LibraryVersions.jwt
    val JwtCirce         = "com.github.jwt-scala" %% "jwt-circe"    % LibraryVersions.jwt
    val Slf4j            = "org.slf4j"             % "slf4j-api"    % LibraryVersions.slf4j
    val JuliSlf4j        = "org.slf4j"             % "jul-to-slf4j" % LibraryVersions.slf4j
    val NopSlf4j         = "org.slf4j"             % "slf4j-nop"    % LibraryVersions.slf4j
    val CatsTime         = Def.setting(
      "org.typelevel" %%% "cats-time" % LibraryVersions.catsTime % "compile->compile;test->test"
    )
    val Log4Cats         = Def.setting("org.typelevel" %%% "log4cats-slf4j" % LibraryVersions.log4cats)
    val Log4CatsNoop     =
      Def.setting("org.typelevel" %%% "log4cats-noop" % LibraryVersions.log4cats % "test")
    val Logback          = Seq(
      "ch.qos.logback"      % "logback-core"    % LibraryVersions.logback,
      "ch.qos.logback"      % "logback-classic" % LibraryVersions.logback,
      "org.codehaus.janino" % "janino"          % LibraryVersions.janino
    )
    val Log4s            = Def.setting("org.log4s" %%% "log4s" % LibraryVersions.log4s)
    val Log4CatsLogLevel = Def.setting(
      Seq(
        "org.typelevel" %%% "log4cats-core"     % LibraryVersions.log4cats,
        "com.rpiaggio"  %%% "log4cats-loglevel" % LibraryVersions.log4catsLevel
      )
    )
    val Logging          = Def.setting(Seq(JuliSlf4j, Log4s.value) ++ Logback)
    val PureConfig       = Seq(
      "com.github.pureconfig" %% "pureconfig"             % LibraryVersions.pureConfig,
      "com.github.pureconfig" %% "pureconfig-cats"        % LibraryVersions.pureConfig,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % LibraryVersions.pureConfig,
      "com.github.pureconfig" %% "pureconfig-http4s"      % LibraryVersions.pureConfig
    )
    val OpenCSV          = "net.sf.opencsv"        % "opencsv"      % LibraryVersions.opencsv
    val Squants          = Def.setting("org.typelevel" %%% "squants" % LibraryVersions.squants)
    val ScalaXml         =
      Def.setting("org.scala-lang.modules" %%% "scala-xml" % LibraryVersions.scalaXmlVersion)
    val Http4s           = Seq("org.http4s" %% "http4s-dsl" % LibraryVersions.http4sVersion,
                     "org.http4s" %% "http4s-blaze-server" % LibraryVersions.http4sBlazeVersion
    )
    val Http4sClient     = Seq(
      "org.http4s" %% "http4s-dsl"             % LibraryVersions.http4sVersion,
      "org.http4s" %% "http4s-jdk-http-client" % LibraryVersions.http4sJdkHttpClientVersion
    )
    val Http4sBoopickle  =
      "org.http4s" %% "http4s-boopickle" % LibraryVersions.http4sBoopickleVersion
    val Http4sCore   = "org.http4s" %% "http4s-core"        % LibraryVersions.http4sVersion
    val Http4sServer = "org.http4s" %% "http4s-server"      % LibraryVersions.http4sVersion
    val Http4sCirce  = "org.http4s" %% "http4s-circe"       % LibraryVersions.http4sVersion
    val Http4sXml    = "org.http4s" %% "http4s-scala-xml-1" % LibraryVersions.http4sScalaXmlVersion
    val Monocle      = Def.setting(
      Seq(
        "dev.optics" %%% "monocle-core"   % LibraryVersions.monocleVersion,
        "dev.optics" %%% "monocle-macro"  % LibraryVersions.monocleVersion,
        "dev.optics" %%% "monocle-unsafe" % LibraryVersions.monocleVersion,
        "dev.optics" %%% "monocle-law"    % LibraryVersions.monocleVersion
      )
    )
    val Circe        = Def.setting(
      Seq(
        "io.circe" %%% "circe-core"    % LibraryVersions.circeVersion,
        "io.circe" %%% "circe-generic" % LibraryVersions.circeVersion,
        "io.circe" %%% "circe-parser"  % LibraryVersions.circeVersion,
        "io.circe" %%% "circe-testing" % LibraryVersions.circeVersion % "test"
      )
    )

    // Client Side JS libraries
    val ReactScalaJS            = Def.setting(
      Seq(
        "com.github.japgolly.scalajs-react" %%% "core"               % LibraryVersions.scalajsReact,
        "com.github.japgolly.scalajs-react" %%% "extra"              % LibraryVersions.scalajsReact,
        "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3" % LibraryVersions.scalajsReact,
        "com.github.japgolly.scalajs-react" %%% "core-ext-cats"      % LibraryVersions.scalajsReact
      )
    )
    val Diode                   = Def.setting(
      Seq(
        "io.suzaku" %%% "diode"       % LibraryVersions.diode,
        "io.suzaku" %%% "diode-react" % LibraryVersions.diode
      )
    )
    val ScalaJSDom              = Def.setting("org.scala-js" %%% "scalajs-dom" % LibraryVersions.scalaDom)
    val ScalaJSReactCommon      =
      Def.setting("io.github.cquiroz.react" %%% "common" % LibraryVersions.scalaJSReactCommon)
    val ScalaJSReactCats        =
      Def.setting("io.github.cquiroz.react" %%% "cats" % LibraryVersions.scalaJSReactCommon)
    val ScalaJSReactSemanticUI  = Def.setting(
      "io.github.cquiroz.react" %%% "react-semantic-ui" % LibraryVersions.scalaJSSemanticUI
    )
    val ScalaJSReactVirtualized = Def.setting(
      "io.github.cquiroz.react" %%% "react-virtualized" % LibraryVersions.scalaJSReactVirtualized
    )
    val ScalaJSReactDraggable   = Def.setting(
      "io.github.cquiroz.react" %%% "react-draggable" % LibraryVersions.scalaJSReactDraggable
    )
    val ScalaJSReactSortable    = Def.setting(
      "io.github.cquiroz.react" %%% "react-sortable-hoc" % LibraryVersions.scalaJSReactSortable
    )
    val ScalaJSReactClipboard   = Def.setting(
      "io.github.cquiroz.react" %%% "react-clipboard" % LibraryVersions.scalaJSReactClipboard
    )
    val BooPickle               = Def.setting("io.suzaku" %%% "boopickle" % LibraryVersions.booPickle)
    val JavaTimeJS              =
      Def.setting("io.github.cquiroz" %%% "scala-java-time" % LibraryVersions.javaTimeJS)
    val GeminiLocales           =
      Def.setting("edu.gemini" %%% "gemini-locales" % LibraryVersions.geminiLocales)
    val PPrint                  = Def.setting("com.lihaoyi" %%% "pprint" % LibraryVersions.pprint)

    // OCS Libraries, these should become modules in the future
    val SpModelCore = "edu.gemini.ocs" %% "edu-gemini-spmodel-core" % LibraryVersions.ocsVersion
    val SeqexecOdb  = Seq(
      "edu.gemini.ocs" %% "edu-gemini-seqexec-odb" % LibraryVersions.ocsVersion,
      ("dom4j"          % "dom4j"                  % "1.6.1")
        .exclude("jaxen", "jaxen")
        .exclude("jaxme", "jaxme-api")
        .exclude("msv", "xsdlib")
        .exclude("msv", "relaxngDatatype")
        .exclude("pull-parser", "pull-parser")
        .exclude("stax", "stax")
        .exclude("xml-apis", "xml-apis")
        .exclude("xpp3", "xpp3")
    )
    val POT         = "edu.gemini.ocs" %% "edu-gemini-pot"          % LibraryVersions.ocsVersion
    val TRPC        = "edu.gemini.ocs" %% "edu-gemini-util-trpc"    % LibraryVersions.ocsVersion
    val WDBAClient  = Seq(
      "edu.gemini.ocs"   %% "edu-gemini-wdba-session-client" % LibraryVersions.ocsVersion,
      "org.apache.xmlrpc" % "xmlrpc-client"                  % LibraryVersions.apacheXMLRPC
    )

    val JAXB = Seq(
      "javax.xml.bind"     % "jaxb-api"     % LibraryVersions.jaxb,
      "org.glassfish.jaxb" % "jaxb-runtime" % LibraryVersions.jaxb,
      "org.glassfish.jaxb" % "jaxb-xjc"     % LibraryVersions.jaxb
    )

    // GIAPI Libraries
    val GiapiScala = "edu.gemini"      %% "giapi" % LibraryVersions.giapiScala
    val Guava      = "com.google.guava" % "guava" % LibraryVersions.guava
    val ACM        = "edu.gemini"      %% "acm"   % LibraryVersions.acm

    // Lucuma Libraries
    val LucumaCore = Def.setting(
      Seq(
        "edu.gemini" %%% "lucuma-core"         % LibraryVersions.lucumaCore,
        "edu.gemini" %%% "lucuma-core-testkit" % LibraryVersions.lucumaCore
      )
    )
    val LucumaUI   = Def.setting("edu.gemini" %%% "lucuma-ui" % LibraryVersions.lucumaUI)

    val Sttp = Def.setting(
      Seq(
        "com.softwaremill.sttp.client3" %%% "core"  % LibraryVersions.sttp,
        "com.softwaremill.sttp.client3" %%% "circe" % LibraryVersions.sttp,
        "com.softwaremill.sttp.client3" %%% "cats"  % LibraryVersions.sttp
      )
    )
  }

  object PluginVersions {
    // Compiler plugins
    val kpVersion        = "0.13.2"
    val betterMonadicFor = "0.3.1"
  }

  object Plugins {
    val kindProjectorPlugin =
      ("org.typelevel" % "kind-projector" % PluginVersions.kpVersion).cross(CrossVersion.full)
    val betterMonadicForPlugin =
      "com.olegpy" %% "better-monadic-for" % PluginVersions.betterMonadicFor
  }

}
