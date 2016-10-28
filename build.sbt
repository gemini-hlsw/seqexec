
lazy val argonautVersion          = "6.2-M3"
lazy val kpVersion                = "0.8.0"
lazy val doobieVersion            = "0.3.0" // TODO
lazy val scalazVersion            = "7.2.4"
lazy val shapelessVersion         = "2.3.1"
lazy val argonautShapelessVersion = "1.2.0-M1"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kpVersion),
  libraryDependencies += "org.scala-lang" %  "scala-reflect" % scalaVersion.value
)

lazy val flywaySettings = Seq(
  flywayUrl             := "jdbc:postgresql:gem",
  flywayUser            := "postgres",
  flywayLocations       := Seq("filesystem:sql")
)

// N.B. `describe` is not a project yet. It doesn't quite compile. Will need some rejiggering.

lazy val gem = project
  .in(file("."))
  .settings(scalaVersion := "2.11.8")
  .aggregate(core, db, importer, json, service, telnetd)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core" % scalazVersion,
      "com.chuusai" %% "shapeless"   % shapelessVersion
    ),
    sourceGenerators in Compile +=
      Def.task { gen2(sourceManaged.value / "gem").unsafePerformIO }.taskValue
  )

lazy val db = project
  .in(file("modules/db"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"               % doobieVersion,
      "org.tpolecat" %% "doobie-contrib-postgresql" % doobieVersion
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
      |import xa.yolo._
    """.stripMargin.trim
  )

lazy val importer = project
  .in(file("modules/importer"))
  .dependsOn(core, db)
  .settings(commonSettings)
  .settings(
    // IDEA needs to see these but sbt doesn't. Go figure.
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"                % "1.0.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    )
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

lazy val service = project
  .in(file("modules/service"))
  .dependsOn(core, db)
  .settings(commonSettings)

lazy val sql = project
  .in(file("sql"))
  .settings(flywaySettings)

lazy val telnetd = project
  .in(file("modules/telnetd"))
  .dependsOn(service)
  .settings(commonSettings)
  .settings(
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.tpolecat" %% "tuco-core" % "0.1.0"
  )
