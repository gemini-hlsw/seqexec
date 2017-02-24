resolvers in ThisBuild +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val argonautVersion          = "6.2-M3"
lazy val doobieVersion            = "0.4.2-SNAPSHOT"
lazy val kpVersion                = "0.9.3"
lazy val scalazVersion            = "7.2.4"
lazy val shapelessVersion         = "2.3.1"
lazy val argonautShapelessVersion = "1.2.0-M1"
lazy val scalaTestVersion         = "3.0.0"
lazy val scalaCheckVersion        = "1.13.1"
lazy val http4sVersion            = "0.15.2a"

enablePlugins(GitVersioning)

git.uncommittedSignifier in ThisBuild := Some("UNCOMMITTED")

lazy val testLibs = Seq(
  "org.scalatest"  %% "scalatest"  % scalaTestVersion  % "test",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

lazy val commonSettings = Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.11",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    // "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused-import",
    "-Ypartial-unification"
  ),
  scalacOptions in (Compile, console) ~= (_.filterNot(Set(
    "-Xfatal-warnings",
    "-Ywarn-unused-import"
  ))),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kpVersion),
  libraryDependencies ++= ("org.scala-lang" %  "scala-reflect" % scalaVersion.value +: testLibs),
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
  .aggregate(core, db, importer, json, ocs2, service, telnetd, ctl)

lazy val core = project
  .in(file("modules/core"))
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
  .dependsOn(core)
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
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.argonaut"                %% "argonaut"               % argonautVersion,
      "io.argonaut"                %% "argonaut-scalaz"        % argonautVersion,
      "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % argonautShapelessVersion
    )
  )

lazy val ocs2 = project
  .in(file("modules/ocs2"))
  .dependsOn(core, db)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"                % "1.0.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.http4s"             %% "http4s-dsl"               % http4sVersion,
      "org.http4s"             %% "http4s-scala-xml"         % http4sVersion,
      "org.http4s"             %% "http4s-blaze-client"      % http4sVersion,
      "org.http4s"             %% "http4s-blaze-server"      % http4sVersion
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .dependsOn(core, db)
  .settings(commonSettings)

lazy val sql = project
  .in(file("modules/sql"))
  .settings(commonSettings ++ flywaySettings)
  .settings(
    libraryDependencies += "org.flywaydb" % "flyway-core" % "4.0.3"
  )

lazy val telnetd = project
  .in(file("modules/telnetd"))
  .dependsOn(service, sql)
  .enablePlugins(JavaAppPackaging)
  .settings(commonSettings)
  .settings(resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven")
  .settings(
    libraryDependencies += "org.tpolecat" %% "tuco-core" % "0.1.0",
    dockerExposedPorts  := List(6666)
  )

lazy val ctl = project
  .in(file("modules/ctl"))
  .settings(commonSettings)
  .settings (
    resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven",
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core"   % scalazVersion,
      "org.scalaz"  %% "scalaz-effect" % scalazVersion,
      "net.bmjames" %% "scala-optparse-applicative" % "0.5"
    ),
    addCommandAlias("gemctl", "ctl/runMain main")
  )
