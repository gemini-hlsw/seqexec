
// The build build, for codegen
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"               % "0.3.0",
  "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
