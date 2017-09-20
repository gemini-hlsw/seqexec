resolvers  += "Flyway" at "https://flywaydb.org/repo"

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "42.1.1", // needed by flyway
  "org.slf4j"      % "slf4j-nop"  % "1.7.21"  // to silence some log messages
)

// addSbtPlugin("org.flywaydb"      % "flyway-sbt"            % "4.2.0") // https://github.com/flyway/flyway/issues/1741
addSbtPlugin("org.scalastyle"   %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"   % "1.2.2")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"               % "0.9.3")
addSbtPlugin("de.heikoseeberger" % "sbt-header"            % "3.0.2")
addSbtPlugin("org.wartremover"   % "sbt-wartremover"       % "2.2.0")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"           % "0.6.20")
// addSbtPlugin("net.virtual-void"  % "sbt-dependency-graph"  % "0.8.2") // https://github.com/jrudolph/sbt-dependency-graph/issues/134
addSbtPlugin("com.timushev.sbt"  % "sbt-updates"           % "0.3.1")
