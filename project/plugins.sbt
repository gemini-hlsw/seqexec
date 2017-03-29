resolvers  += "Flyway" at "https://flywaydb.org/repo"

addSbtPlugin("org.flywaydb"     % "flyway-sbt"            % "4.0.3")
addSbtPlugin("org.scalastyle"  %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager"   % "1.1.4")
addSbtPlugin("com.typesafe.sbt" % "sbt-git"               % "0.8.5")

// To silence SLF4J warnings
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
