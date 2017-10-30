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
      // "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates",            // Warn if a private member is unused.
      "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
      "-Yinduction-heuristics",            // speeds up the compilation of inductive implicit resolution
      // "-Ykind-polymorphism",               // type and method definitions with type parameters of arbitrary kinds
      "-Yliteral-types",                   // literals can appear in type position
      "-Xstrict-patmat-analysis",          // more accurate reporting of failures of match exhaustivity
      "-Xlint:strict-unsealed-patmat"      // warn on inexhaustive matches against unsealed traits
    )
  }

  /** Library versions */
  object LibraryVersions {
    val scalaCommonVersion = "2.12.3"
    val scalaVersion       = s"$scalaCommonVersion-bin-typelevel-4"

    // ScalaJS libraries
    val scalaDom     = "0.9.3"
    val scalajsReact = "1.1.1"
    val scalaCSS     = "0.5.3"
    val booPickle    = "1.2.6"
    val diode        = "1.1.2"
    val javaTimeJS   = "0.2.2"
    val javaLogJS    = "0.1.2"
    val scalaJQuery  = "1.2"

    // Java libraries
    val scalaZ       = "7.2.16"
    val scalaZStream = "0.8.6a"

    // Scala libraries
    val http4s       = "0.16.5a"
    val squants      = "1.3.0"
    val argonaut     = "6.2"
    val commonsHttp  = "2.0.2"
    val unboundId    = "3.2.1"
    val jwt          = "0.14.0"
    val slf4j        = "1.7.25"
    val log4s        = "1.4.0"
    val logback      = "1.2.3"
    val janino       = "3.0.7"
    val knobs        = "4.0.31-scalaz-7.2"
    val monocle      = "1.4.0"

    // test libraries
    val scalaTest             = "3.0.4"
    val scalaCheck            = "1.13.5"
    val scalaCheckShapeless   = "1.1.7"
    val discipline            = "0.8"

    // Pure JS libraries
    val reactJS        = "15.6.1"
    val jQuery         = "3.2.1"
    val semanticUI     = "2.2.10"
    val ocsVersion     = "2017101.1.4"
    val uglifyJs       = "1.0.0-beta.3"

    //Apache XMLRPC
    val apacheXMLRPC   = "3.1.3"

    val opencsv        = "2.1"
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
      "com.github.alexarchambault" %%% "scalacheck-shapeless_1.13" % LibraryVersions.scalaCheckShapeless % "test",
      "org.typelevel"              %%% "discipline"                % LibraryVersions.discipline          % "test"
    ))

    val Argonaut    = "io.argonaut"        %% "argonaut"                          % LibraryVersions.argonaut
    val CommonsHttp = "commons-httpclient" %  "commons-httpclient"                % LibraryVersions.commonsHttp
    val UnboundId   = "com.unboundid"      %  "unboundid-ldapsdk-minimal-edition" % LibraryVersions.unboundId
    val JwtCore     = "com.pauldijou"      %% "jwt-core"                          % LibraryVersions.jwt
    val JuliSlf4j   = "org.slf4j"          %  "jul-to-slf4j"                      % LibraryVersions.slf4j
    val Logback     = Seq(
      "ch.qos.logback"      % "logback-core"    % LibraryVersions.logback,
      "ch.qos.logback"      % "logback-classic" % LibraryVersions.logback,
      "org.codehaus.janino" % "janino"          % LibraryVersions.janino
    )
    val Log4s       = "org.log4s"          %% "log4s"                             % LibraryVersions.log4s
    val Logging     = Seq(JuliSlf4j, Log4s) ++ Logback
    val Knobs       = "io.verizon.knobs"   %% "core"                              % LibraryVersions.knobs
    val OpenCSV     = "net.sf.opencsv"     %  "opencsv"                           % LibraryVersions.opencsv

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

    val Monocle  = Def.setting(Seq(
      "com.github.julien-truffaut" %%% "monocle-core"  % LibraryVersions.monocle,
      "com.github.julien-truffaut" %%% "monocle-macro" % LibraryVersions.monocle,
      "com.github.julien-truffaut" %%% "monocle-law"   % LibraryVersions.monocle % "test"))

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
    val SeqexecOdb  = Seq(
                      "edu.gemini.ocs"    %% "edu-gemini-seqexec-odb"         % LibraryVersions.ocsVersion,
                      "dom4j"            %  "dom4j"                          % "1.5.1"
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
                      "org.apache.xmlrpc" %  "xmlrpc-client"                  % LibraryVersions.apacheXMLRPC
    )
  }

  object PluginVersions {
    // Compiler plugins
    val paradiseVersion    = "2.1.1"
  }

  object Plugins {
    val paradisePlugin = "org.scalamacros" %% "paradise" % PluginVersions.paradiseVersion cross CrossVersion.patch
  }

}
