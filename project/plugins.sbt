resolvers  ++= Seq(
  "Flyway" at "https://davidmweber.github.io/flyway-sbt.repo",
  Resolver.bintrayIvyRepo("rtimush", "sbt-plugin-snapshots")
)
// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.32")

addSbtPlugin("edu.gemini"         % "sbt-gsp"                  % "0.1.15")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"           % "sbt-revolver"             % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.9.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.7.1")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"               % "4.0.0")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler-sjs06"% "0.17.0")

// Used to find dependencies
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph"     % "0.9.2")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.5.0")

onLoad in Global := { s => "dependencyUpdates" :: s }
