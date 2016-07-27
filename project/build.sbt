
// The build build, for codegen
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"               % "0.3.0-M1", // uh, lame
  "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0-M1"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
