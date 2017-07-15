resolvers  += "Flyway" at "https://flywaydb.org/repo"

addSbtPlugin("org.flywaydb"      % "flyway-sbt"            % "4.0.3")
addSbtPlugin("org.scalastyle"   %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"   % "1.1.4")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"               % "0.8.5")
// addSbtPlugin("io.get-coursier"   % "sbt-coursier"          % "1.0.0-RC4")
addSbtPlugin("de.heikoseeberger" % "sbt-header"            % "2.0.0")
addSbtPlugin("org.wartremover"   % "sbt-wartremover"       % "2.1.1")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"           % "0.6.18")

// To silence SLF4J warnings
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
