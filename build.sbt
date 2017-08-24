resolvers in ThisBuild +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val circeVersion        = "0.9.0-M1"
lazy val attoVersion         = "0.6.1-M1"
lazy val catsEffectVersion   = "0.4"
lazy val catsVersion         = "1.0.0-MF"
lazy val declineVersion      = "0.4.0-M1"
lazy val doobieVersion       = "0.5.0-M2"
lazy val flywayVersion       = "4.0.3"
lazy val fs2Version          = "0.10.0-M5"
lazy val http4sVersion       = "0.18.0-SNAPSHOT"
lazy val jwtVersion          = "0.14.0"
lazy val kpVersion           = "0.9.3"
lazy val mouseVersion        = "0.10-MF"
lazy val scalaCheckVersion   = "1.13.5"
lazy val scalaParsersVersion = "1.0.4"
lazy val scalaTestVersion    = "3.0.1"
lazy val scalaXmlVerson      = "1.0.6"
lazy val shapelessVersion    = "2.3.2"
lazy val slf4jVersion        = "1.7.25"
lazy val tucoVersion         = "0.3.0-M2"

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

lazy val testLibs = Def.setting(Seq(
  "org.scalatest"  %%% "scalatest"  % scalaTestVersion  % "test",
  "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % "test"
))

lazy val gemWarts =
  Warts.allBut(
    Wart.Any,                // false positives
    Wart.Nothing,            // false positives
    Wart.Null,               // false positives
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

  // Temporary, needed for decline 0.4.0-M1
  resolvers += Resolver.jcenterRepo,

  // Wartremover in compile and test (not in Console)
  wartremoverErrors in (Compile, compile) := gemWarts,
  wartremoverErrors in (Test,    compile) := gemWarts,

  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.3-bin-typelevel-4",
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
  scalacOptions in (Compile, doc) ++= Seq(
    "-groups",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-skip-packages", "scalaz",
    "-doc-title", "Gem",
    "-doc-version", version.value
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kpVersion),
  libraryDependencies ++= (scalaOrganization.value % "scala-reflect" % scalaVersion.value +: testLibs.value),
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
  .aggregate(coreJVM, db, json, ocs2, service, telnetd, ctl, web)

lazy val core = crossProject
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"           %%% "cats-core"    % catsVersion,
      "org.typelevel"           %%% "cats-testkit" % catsVersion % "test",
      "com.chuusai"             %%% "shapeless"    % shapelessVersion,
      "org.tpolecat"            %%% "atto-core"    % attoVersion,
      "com.github.benhutchison" %%% "mouse"        % mouseVersion
    )
  )
  .jsSettings(
    libraryDependencies +=
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M12",
    // These settings allow to use TLS with scala.js
    // Remove the dependency on the scalajs-compiler
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler"),
    // And add a custom one
    addCompilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.patch),
    // Make JS tests run fine on travis
    parallelExecution in Test := false
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val db = project
  .in(file("modules/db"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"      % doobieVersion,
      "org.tpolecat" %% "doobie-postgres"  % doobieVersion,
      "org.tpolecat" %% "doobie-scalatest" % doobieVersion % "test"
    ),
    initialCommands += """
      |import cats._, cats.data._, cats.implicits._, scalaz.effect.IO
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
  .dependsOn(coreJVM)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser"  % circeVersion
    )
  )

lazy val sql = project
  .in(file("modules/sql"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings ++ flywaySettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.flywaydb" %  "flyway-core"      % flywayVersion,
      "org.tpolecat" %% "doobie-core"      % doobieVersion,
      "org.tpolecat" %% "doobie-postgres"  % doobieVersion
    ),
    addCommandAlias("genEnums", "; sql/runMain gem.sql.Main modules/core/src/main/scala/gem/enum; headerCreate")
  )

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM, db, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"                 %% "fs2-io"                   % fs2Version,
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
  .dependsOn(coreJVM, db)
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
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "tuco-core" % tucoVersion,
      "org.tpolecat" %% "tuco-shell" % tucoVersion
    ),
    dockerExposedPorts  := List(6666),
    dockerRepository    := Some("geminihlsw")
  )

lazy val web = project
  .in(file("modules/web"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql, json)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.slf4j"      % "slf4j-jdk14"         % slf4jVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "com.pauldijou" %% "jwt-core"            % jwtVersion
    ),
    dockerExposedPorts  := List(6667),
    dockerRepository    := Some("geminihlsw")
  )

lazy val ctl = project
  .in(file("modules/ctl"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings (
    resolvers += Resolver.bintrayRepo("bkirwi", "maven"),
    libraryDependencies ++= Seq(
      "org.typelevel"           %% "cats-core"   % catsVersion,
      "org.typelevel"           %% "cats-free"   % catsVersion,
      "org.typelevel"           %% "cats-effect" % catsEffectVersion,
      "com.monovore"            %% "decline"     % declineVersion,
      "com.github.benhutchison" %% "mouse"       % mouseVersion
    ),
    addCommandAlias("gemctl", "ctl/runMain gem.ctl.main")
  )
