
lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"               % "0.3.0-M1", // uh, lame
      "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0-M1"
    ),
    initialCommands := """
    import edu.gemini.spModel.core._
    import gem._
    """,
    // sourceGenerators in Compile +=
    //   Def.task { gen(sourceManaged.value / "gem").unsafePerformIO }.taskValue,
    sourceGenerators in Compile +=
      Def.task { gen2(sourceManaged.value / "gem").unsafePerformIO }.taskValue
  )

lazy val importer = project
  .in(file("modules/importer"))
  .dependsOn(core)
  .settings(commonSettings)
