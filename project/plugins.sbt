resolvers  ++= Seq(
  "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo",
  Resolver.bintrayIvyRepo("rtimush", "sbt-plugin-snapshots")
)
// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.26")

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql"  % "42.2.1", // needed by flyway
  "org.slf4j"      % "slf4j-nop"   % "1.7.25", // to silence some log messages
  "org.typelevel" %% "cats-core"   % "1.0.1",
  "org.typelevel" %% "cats-effect" % "0.8"
)
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)

addSbtPlugin("org.flywaydb"       % "flyway-sbt"               % "4.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"           % "sbt-revolver"             % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.7.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.3.5")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0")

// add and check headers
addSbtPlugin("de.heikoseeberger"  % "sbt-header"               % "4.1.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "0.9.3")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"               % "3.0.0")

addSbtPlugin("org.wartremover"    % "sbt-wartremover"          % "2.3.7")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.14.0")

// Used to find dependencies
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph"     % "0.9.0")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.3.4")

onLoad in Global := { s => "dependencyUpdates" :: s }
