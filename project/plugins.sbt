resolvers  ++= Seq(
  "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo",
  Resolver.bintrayIvyRepo("rtimush", "sbt-plugin-snapshots")
)
// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.3.0")

addSbtPlugin("edu.gemini"         % "sbt-gsp"                  % "0.2.5")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"           % "sbt-revolver"             % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.10.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.7.6")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"               % "4.1.1")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.18.0")

// Used to find dependencies
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph"     % "0.9.2")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.5.1")

onLoad in Global := { s => "dependencyUpdates" :: s }
