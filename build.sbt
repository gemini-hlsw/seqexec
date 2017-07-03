resolvers in ThisBuild +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val argonautVersion          = "6.2-RC2"
lazy val doobieVersion            = "0.4.2-SNAPSHOT"
lazy val kpVersion                = "0.9.3"
lazy val scalazVersion            = "7.2.13"
lazy val shapelessVersion         = "2.3.2"
lazy val argonautShapelessVersion = "1.2.0-M4"
lazy val scalaTestVersion         = "3.0.1"
lazy val scalaCheckVersion        = "1.13.5"
lazy val http4sVersion            = "0.15.2a"
lazy val scalaXmlVerson           = "1.0.6"
lazy val scalaParsersVersion      = "1.0.4"
lazy val tucoVersion              = "0.1.1"

enablePlugins(GitVersioning)

git.uncommittedSignifier in ThisBuild := Some("UNCOMMITTED")

// Before printing the prompt check git to make sure all is well.
shellPrompt in ThisBuild := { state =>
  import scala.sys.process._
  import scala.Console.{ RED, RESET }
  try {
    val revision = "git rev-parse HEAD".!!.trim
    val dirty    = "git status -s".!!.trim.length > 0
    val expected = revision + git.uncommittedSignifier.value.filter(_ => dirty).fold("")("-" + _)
    val actual   = version.value
    val stale    = expected != actual
    if (stale) {
      print(RED)
      println(s"Computed version doesn't match the filesystem anymore.")
      println(s"Please `reload` to get back in sync.")
      print(RESET)
    }
  } catch {
    case e: Exception =>
      print(RED)
      println(s"Couldn't run `git` to check on versioning. Something is amiss.")
      print(RESET)
  }
  "> "
}

// sbt-header requires these settings even though we're using a custom license header
organizationName in ThisBuild := "Association of Universities for Research in Astronomy, Inc. (AURA)"
startYear        in ThisBuild := Some(2017)
licenses         in ThisBuild += ("BSD-3-Clause", new URL("https://opensource.org/licenses/BSD-3-Clause"))

lazy val testLibs = Seq(
  "org.scalatest"  %% "scalatest"  % scalaTestVersion  % "test",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

lazy val gemWarts =
  Warts.allBut(
    Wart.Any,                // false positives
    Wart.Nothing,            // false positives
    Wart.Product,            // false positives
    Wart.Serializable,       // false positives
    Wart.Recursion,          // false positives
    Wart.ImplicitConversion  // we know what we're doing
  )

lazy val commonSettings = Seq(

  // These sbt-header settings can't be set in ThisBuild for some reason
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.CppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
       |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
       |""".stripMargin
  )),

  // Wartremover in compile and test (not in Console)
  wartremoverErrors in (Compile, compile) := gemWarts,
  wartremoverErrors in (Test,    compile) := gemWarts,

  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.2-bin-typelevel-4",
  scalacOptions ++= Seq(
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
  ),
  scalacOptions in (Compile, console) ~= (_.filterNot(Set(
    "-Xfatal-warnings",
    "-Ywarn-unused:imports"
  ))),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kpVersion),
  libraryDependencies ++= (scalaOrganization.value %  "scala-reflect" % scalaVersion.value +: testLibs),
  name := "gem-" + name.value
)

lazy val flywaySettings = Seq(
  flywayUrl  := "jdbc:postgresql:gem",
  flywayUser := "postgres",
  flywayLocations := Seq(
    s"filesystem:${baseDirectory.value}/src/main/resources/db/migration"
  )
)

// N.B. `describe` is not a project yet. It doesn't quite compile. Will need some rejiggering.

lazy val gem = project
  .in(file("."))
  .settings(scalaVersion := "2.11.8")
  .aggregate(core, db, json, ocs2, service, telnetd, ctl)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core" % scalazVersion,
      "com.chuusai" %% "shapeless"   % shapelessVersion
    ),
    sourceGenerators in Compile +=
      Def.task { gen2((sourceManaged in Compile).value / "gem").unsafePerformIO }.taskValue
  )

lazy val db = project
  .in(file("modules/db"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"      % doobieVersion,
      "org.tpolecat" %% "doobie-postgres"  % doobieVersion,
      "org.tpolecat" %% "doobie-scalatest" % doobieVersion % "test"
    ),
    initialCommands += """
      |import scalaz._, Scalaz._, scalaz.effect.IO
      |import doobie.imports._
      |import gem._, gem.enum._, gem.dao._
      |val xa = DriverManagerTransactor[IO](
      |  "org.postgresql.Driver",
      |  "jdbc:postgresql:gem",
      |  "postgres",
      |  "")
      |val y = xa.yolo
      |import y._
    """.stripMargin.trim
  )

lazy val json = project
  .in(file("modules/json"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.argonaut"                %% "argonaut"               % argonautVersion,
      "io.argonaut"                %% "argonaut-scalaz"        % argonautVersion,
      "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % argonautShapelessVersion
    )
  )

lazy val sql = project
  .in(file("modules/sql"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings ++ flywaySettings)
  .settings(
    libraryDependencies += "org.flywaydb" % "flyway-core" % "4.0.3"
  )

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, db, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"                % scalaXmlVerson,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParsersVersion,
      "org.http4s"             %% "http4s-dsl"               % http4sVersion,
      "org.http4s"             %% "http4s-scala-xml"         % http4sVersion,
      "org.http4s"             %% "http4s-blaze-client"      % http4sVersion,
      "org.http4s"             %% "http4s-blaze-server"      % http4sVersion
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, db)
  .settings(commonSettings)

lazy val telnetd = project
  .in(file("modules/telnetd"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(commonSettings)
  .settings(resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven")
  .settings(
    libraryDependencies += "org.tpolecat" %% "tuco-core" % tucoVersion,
    dockerExposedPorts  := List(6666),
    dockerRepository    := Some("geminihlsw")
  )

lazy val ctl = project
  .in(file("modules/ctl"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings (
    resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven",
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core"   % scalazVersion,
      "org.scalaz"  %% "scalaz-effect" % scalazVersion,
      "net.bmjames" %% "scala-optparse-applicative" % "0.5"
    ),
    addCommandAlias("gemctl", "ctl/runMain gem.ctl.main")
  )
