
lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0"),
  libraryDependencies += "org.scala-lang" %  "scala-reflect" % scalaVersion.value
)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core" % "7.2.4",
      "com.chuusai" %% "shapeless"   % "2.3.1"
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
      "org.tpolecat" %% "doobie-core"               % "0.3.0-M1", // uh, lame
      "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0-M1"
    )
  )

lazy val importer = project
  .in(file("modules/importer"))
  .dependsOn(core, db)
  .settings(commonSettings)
