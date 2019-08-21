resolvers  ++= Seq(
  "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo",
  Resolver.bintrayIvyRepo("rtimush", "sbt-plugin-snapshots")
)
// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.28")

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql"  % "42.2.6", // needed by flyway
  "org.slf4j"      % "slf4j-nop"   % "1.7.28", // to silence some log messages
  "org.typelevel" %% "cats-core"   % "2.0.0-RC1",
  "org.typelevel" %% "cats-effect" % "2.0.0-RC1"
)

addSbtPlugin("edu.gemini"         % "sbt-gsp"                  % "0.1.9")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)

addSbtPlugin("org.flywaydb"       % "flyway-sbt"               % "4.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"           % "sbt-revolver"             % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.9.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.4.1")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"               % "4.0.0")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.15.0-0.6")

// Used to find dependencies
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph"     % "0.9.2")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.4.2")

onLoad in Global := { s => "dependencyUpdates" :: s }
