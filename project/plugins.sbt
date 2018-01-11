resolvers  ++= Seq(
  "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo",
  Resolver.bintrayIvyRepo("rtimush", "sbt-plugin-snapshots")
)

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql"  % "42.1.1", // needed by flyway
  "org.slf4j"      % "slf4j-nop"   % "1.7.21", // to silence some log messages
  "org.typelevel" %% "cats-core"   % "1.0.0-MF",
  "org.typelevel" %% "cats-effect" % "0.4"
)

addSbtPlugin("org.flywaydb"      % "flyway-sbt"            % "4.2.0")
addSbtPlugin("org.scalastyle"   %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"   % "1.2.2")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"               % "0.9.3")
addSbtPlugin("de.heikoseeberger" % "sbt-header"            % "3.0.2")
addSbtPlugin("org.wartremover"   % "sbt-wartremover"       % "2.2.1")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"           % "0.6.20")
addSbtPlugin("net.virtual-void"  % "sbt-dependency-graph"  % "0.9.0")
addSbtPlugin("com.timushev.sbt"  % "sbt-updates"           % "0.3.4-2+gd4c4ec6")
