import com.typesafe.sbt.packager.docker._

lazy val circeVersion         = "0.9.3"
lazy val attoVersion          = "0.6.2"
lazy val catsEffectVersion    = "0.10.1"
lazy val catsVersion          = "1.1.0"
lazy val declineVersion       = "0.4.1"
lazy val doobieVersion        = "0.5.2"
lazy val flywayVersion        = "5.0.7"
lazy val fs2Version           = "0.10.3"
lazy val http4sVersion        = "0.18.9"
lazy val jwtVersion           = "0.16.0"
lazy val kpVersion            = "0.9.6"
lazy val monocleVersion       = "1.5.1-cats"
lazy val mouseVersion         = "0.16"
lazy val paradiseVersion      = "2.1.1"
lazy val scalaCheckVersion    = "1.13.5"
lazy val scalaParsersVersion  = "1.1.0"
lazy val scalaTestVersion     = "3.0.5"
lazy val scalaXmlVerson       = "1.1.0"
lazy val shapelessVersion     = "2.3.3"
lazy val slf4jVersion         = "1.7.25"
lazy val tucoVersion          = "0.3.1"
lazy val scalaJavaTimeVersion = "2.0.0-M13"

// our version is determined by the current git state (see project/ImageManifest.scala)
def imageManifest = ImageManifest.current("postgres:9.6.0").unsafeRunSync

version in ThisBuild := imageManifest.formatVersion

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  if (sys.props.contains("gem.skipDependencyUpdates")) s
  else "dependencyUpdates" :: s
}

cancelable in Global := true

// some extra commands for us
addCommandAlias("genEnums", "; sql/runMain gem.sql.Main modules/core/shared/src/main/scala/gem/enum")
addCommandAlias("schemaSpy", "sql/runMain org.schemaspy.Main -t pgsql -port 5432 -db gem -o modules/sql/target/schemaspy -u postgres -host localhost -s public")
addCommandAlias("gemctl", "ctl/runMain gem.ctl.main")//

// Before printing the prompt check git to make sure all is well.
shellPrompt in ThisBuild := { state =>
  if (version.value != imageManifest.formatVersion) {
    import scala.Console.{ RED, RESET }
    print(RED)
    println(s"Computed version doesn't match the filesystem anymore.")
    println(s"Please `reload` to get back in sync.")
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
    Wart.Product,            // false positives
    Wart.Serializable,       // false positives
    Wart.Recursion,          // false positives
    Wart.ImplicitConversion, // we know what we're doing
    Wart.ImplicitParameter   // false positives
  )

lazy val commonSettings = Seq(

  // Don't worry about stale deps pulled in by scala-js
  dependencyUpdatesFilter -= moduleFilter(organization = "org.eclipse.jetty"),

  // Don't worry about monocle versions that start with the same prefix.
  dependencyUpdatesFilter -= moduleFilter(
    organization = "com.github.julien-truffaut",
    revision = sbt.io.GlobFilter(monocleVersion.replace("-cats", "*"))
  ),

  // These sbt-header settings can't be set in ThisBuild for some reason
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
       |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
       |""".stripMargin
  )),

  // Don't build javadoc when we're packaging the docker image.
  mappings in (Compile, packageDoc) := Seq(),

  // We don't care to see updates about the scala language itself
  dependencyUpdatesFilter -= moduleFilter(name = "scala-library"),
  dependencyUpdatesFilter -= moduleFilter(name = "scala-reflect"),

  // Temporary, needed for decline 0.4.0-M1
  resolvers += Resolver.jcenterRepo,

  // Wartremover in compile and test (not in Console)
  wartremoverErrors in (Compile, compile) := gemWarts,
  wartremoverErrors in (Test,    compile) := gemWarts,

  scalaVersion := "2.12.5",
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
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
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

  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "gem.test.Tags.RequiresNetwork"), // by default, ignore network tests

  // We need kind-projector generally, and paradise for
  addCompilerPlugin("org.spire-math"  %% "kind-projector" % kpVersion),
  addCompilerPlugin("org.scalamacros" %% "paradise"       % paradiseVersion cross CrossVersion.patch),

  libraryDependencies ++= (scalaOrganization.value % "scala-reflect" % scalaVersion.value +: testLibs.value),
  name := "gem-" + name.value
)

lazy val commonJSSettings = Seq(
  // These settings allow to use TLS with scala.js
  // Remove the dependency on the scalajs-compiler
  libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler"),
  // And add a custom one
  addCompilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.patch),
  // Make JS tests run fine on travis
  parallelExecution in Test := false
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
  .settings(commonSettings)
  .aggregate(coreJVM, coreJS, db, json, ocs2, ephemeris, service, telnetd, ctl, web, sql, main, ui)

lazy val core = crossProject
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"              %%% "cats-core"       % catsVersion,
      "org.typelevel"              %%% "cats-effect"     % catsEffectVersion,
      "org.typelevel"              %%% "cats-testkit"    % catsVersion % "test",
      "org.typelevel"              %%% "mouse"           % mouseVersion,
      "com.chuusai"                %%% "shapeless"       % shapelessVersion,
      "org.tpolecat"               %%% "atto-core"       % attoVersion,
      "com.github.julien-truffaut" %%% "monocle-core"    % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"   % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-law"     % monocleVersion % "test"
    )
  )
  .jsSettings(
    libraryDependencies +=
      "io.github.cquiroz"          %%% "scala-java-time" % scalaJavaTimeVersion,
    wartremoverExcluded += sourceManaged.value / "main" / "java" / "time" / "zone" / "TzdbZoneRulesProvider.scala",
    // We only care about these two timezones. UTC is implicitly included
    zonesFilter         := {(z: String) => z == "America/Santiago" || z == "Pacific/Honolulu"}
  )
  .jsSettings(commonJSSettings)
  .jvmSettings(
    libraryDependencies += "co.fs2" %% "fs2-io" % fs2Version
  )


lazy val coreJVM = core.jvm.enablePlugins(AutomateHeaderPlugin)
lazy val coreJS = core.js.enablePlugins(TzdbPlugin)

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
    )
  )

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM, db, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-testkit"             % catsVersion % "test",
      "co.fs2"                 %% "fs2-io"                   % fs2Version,
      "org.scala-lang.modules" %% "scala-xml"                % scalaXmlVerson,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParsersVersion,
      "org.http4s"             %% "http4s-dsl"               % http4sVersion,
      "org.http4s"             %% "http4s-scala-xml"         % http4sVersion,
      "org.http4s"             %% "http4s-blaze-client"      % http4sVersion,
      "org.http4s"             %% "http4s-blaze-server"      % http4sVersion
    )
  )

lazy val ephemeris = project
  .in(file("modules/ephemeris"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM % "compile->compile;test->test", db, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"        %% "fs2-io"              % fs2Version     % "test",
      "org.http4s"    %% "http4s-blaze-client" % http4sVersion,
      "org.typelevel" %% "cats-testkit"        % catsVersion    % "test"
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM, db, ephemeris, ocs2)
  .settings(commonSettings)

lazy val telnetd = project
  .in(file("modules/telnetd"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "tuco-core"  % tucoVersion,
      "org.tpolecat" %% "tuco-shell" % tucoVersion
    )
  )

lazy val web = project
  .in(file("modules/web"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(service, sql, json)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.slf4j"      % "slf4j-jdk14"         % slf4jVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "com.pauldijou" %% "jwt-core"            % jwtVersion
    )
  )

lazy val ui = project
  .in(file("modules/ui"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)
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
    resolvers += Resolver.bintrayRepo("bkirwi", "maven"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % catsVersion,
      "org.typelevel" %% "cats-free"   % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "com.monovore"  %% "decline"     % declineVersion
    ),
    TaskKey[Unit]("deployTest") := (runMain in Compile).toTask {
      s" gem.ctl.main --no-ansi --host sbfocstest-lv1.cl.gemini.edu deploy-test ${imageManifest.formatVersion}"
    } .value,
    fork in run := true
  )

lazy val main = project
  .in(file("modules/main"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(web, telnetd)
  .settings(commonSettings)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(
    packageName in Docker := "gem",
    dockerBaseImage       := "openjdk:8u141",
    dockerExposedPorts    := List(9090, 9091),
    dockerRepository      := Some("sbfocsdev-lv1.cl.gemini.edu"),
    dockerLabels          := imageManifest.labels,

    // Install nc before changing the user
    dockerCommands       ++= dockerCommands.value.flatMap {
      case c @ Cmd("USER", args @ _*) =>
        Seq(
          ExecCmd("RUN", "apt-get", "update"),
          ExecCmd("RUN", "apt-get", "--assume-yes", "install", "netcat-openbsd"),
          c
        )
      case cmd => Seq(cmd)
    }

  )
