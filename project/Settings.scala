import sbt._

/**
 * Application settings and dependencies
 */
object Settings {
  object Definitions {
    /** The name of the application */
    val name = "ocs3"

    /** Options for the scala compiler */
    val scalacOptions = Seq(
      "-unchecked",
      "-deprecation",
      "-encoding", "UTF-8", // Keep on the same line
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-target:jvm-1.8",
      "-Xlint",
      "-Xlint:-stars-align",
      "-Xfatal-warnings",
      "-Ywarn-unused-import"
    )
  }

  /** Library versions */
  object LibraryVersions {
    val scala        = "2.11.11"

    // ScalaJS libraries
    val scalaDom     = "0.9.2"
    val scalajsReact = "1.0.0"
    val scalaCSS     = "0.5.3"
    val booPickle    = "1.2.6"
    val diode        = "1.1.2"
    val javaTimeJS   = "0.2.1"
    val javaLogJS    = "0.1.1"
    val scalaJQuery  = "1.0"

    // Java libraries
    val scalaZ       = "7.2.13"
    val scalaZStream = "0.8.6a"

    // Scala libraries
    val http4s       = "0.16.0a-M3"
    val squants      = "1.3.0"
    val argonaut     = "6.2"
    val commonsHttp  = "2.0.2"
    val unboundId    = "3.2.1"
    val jwt          = "0.12.1"
    val slf4j        = "1.7.25"
    val knobs        = "4.0.31-scalaz-7.2"
    val monocle      = "1.4.0"

    // test libraries
    val scalaTest             = "3.0.3"
    val scalaCheck            = "1.13.5"
    val scalaCheckShapeless   = "1.1.5"

    // Pure JS libraries
    val reactJS        = "15.5.4"
    val jQuery         = "3.2.1"
    val semanticUI     = "2.2.7"
    val jQueryTerminal = "0.11.2"
    val ocsVersion     = "2017101.1.1"

    //Apache XMLRPC
    val apacheXMLRPC   = "3.1.3"
  }

  /**
    * Global libraries
    */
  object Libraries {
    import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

    // Test Libraries
    val TestLibs = Def.setting(Seq(
      "org.scalatest"              %%% "scalatest"                 % LibraryVersions.scalaTest           % "test",
      "org.scalacheck"             %%% "scalacheck"                % LibraryVersions.scalaCheck          % "test",
      "com.github.alexarchambault" %%% "scalacheck-shapeless_1.13" % LibraryVersions.scalaCheckShapeless % "test"
    ))

    val Argonaut    = "io.argonaut"        %% "argonaut"                          % LibraryVersions.argonaut
    val CommonsHttp = "commons-httpclient" %  "commons-httpclient"                % LibraryVersions.commonsHttp
    val UnboundId   = "com.unboundid"      %  "unboundid-ldapsdk-minimal-edition" % LibraryVersions.unboundId
    val JwtCore     = "com.pauldijou"      %% "jwt-core"                          % LibraryVersions.jwt
    val Slf4jJuli   = "org.slf4j"          %  "slf4j-jdk14"                       % LibraryVersions.slf4j
    val Knobs       = "io.verizon.knobs"   %% "core"                              % LibraryVersions.knobs

    val Squants     = Def.setting("org.typelevel" %%% "squants"              % LibraryVersions.squants)
    val BooPickle   = Def.setting("io.suzaku"     %%% "boopickle"            % LibraryVersions.booPickle)
    val JavaTimeJS  = Def.setting("org.scala-js"  %%% "scalajs-java-time"    % LibraryVersions.javaTimeJS)
    val JavaLogJS   = Def.setting("org.scala-js"  %%% "scalajs-java-logging" % LibraryVersions.javaLogJS)

    // ScalaZ
    val ScalaZCore       = Def.setting("org.scalaz" %%% "scalaz-core"         % LibraryVersions.scalaZ)
    val ScalaZConcurrent = "org.scalaz"             %%  "scalaz-concurrent"   % LibraryVersions.scalaZ
    val ScalaZStream     = "org.scalaz.stream"      %%  "scalaz-stream"       % LibraryVersions.scalaZStream

    // Server side libraries
    val Http4s  = Seq(
      "org.http4s" %% "http4s-dsl"          % LibraryVersions.http4s,
      "org.http4s" %% "http4s-blaze-server" % LibraryVersions.http4s)

    val Http4sClient  = Seq(
      "org.http4s" %% "http4s-blaze-client" % LibraryVersions.http4s,
      "org.http4s" %% "http4s-scala-xml"    % LibraryVersions.http4s)

    val Monocle  = Def.setting(Seq(
      "com.github.julien-truffaut" %%% "monocle-core"  % LibraryVersions.monocle,
      "com.github.julien-truffaut" %%% "monocle-macro" % LibraryVersions.monocle))

    // Client Side JS libraries
    val ReactScalaJS = Def.setting(Seq(
      "com.github.japgolly.scalajs-react" %%% "core"         % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalajs-react" %%% "extra"        % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalajs-react" %%% "ext-scalaz72" % LibraryVersions.scalajsReact,
      "com.github.japgolly.scalacss"      %%% "ext-react"    % LibraryVersions.scalaCSS
    ))
    val Diode = Def.setting(Seq(
      "io.suzaku" %%% "diode"       % LibraryVersions.diode,
      "io.suzaku" %%% "diode-react" % LibraryVersions.diode
    ))
    val ScalaCSS   = Def.setting("com.github.japgolly.scalacss" %%% "core"          % LibraryVersions.scalaCSS)
    val ScalaJSDom = Def.setting("org.scala-js"                 %%% "scalajs-dom"   % LibraryVersions.scalaDom)
    val JQuery     = Def.setting("org.querki"                   %%% "jquery-facade" % LibraryVersions.scalaJQuery)

    // OCS Libraries, these should become modules in the future
    val SpModelCore = "edu.gemini.ocs"    %% "edu-gemini-spmodel-core"        % LibraryVersions.ocsVersion
    val SeqexecOdb  = "edu.gemini.ocs"    %% "edu-gemini-seqexec-odb"         % LibraryVersions.ocsVersion
    val POT         = "edu.gemini.ocs"    %% "edu-gemini-pot"                 % LibraryVersions.ocsVersion
    val EpicsACM    = "edu.gemini.ocs"    %% "edu-gemini-epics-acm"           % LibraryVersions.ocsVersion
    val TRPC        = "edu.gemini.ocs"    %% "edu-gemini-util-trpc"           % LibraryVersions.ocsVersion
    val WDBAClient  = Seq(
                      "edu.gemini.ocs"    %% "edu-gemini-wdba-session-client" % LibraryVersions.ocsVersion,
                      "org.apache.xmlrpc" %  "xmlrpc-client"                  % LibraryVersions.apacheXMLRPC
    )

  }

}
