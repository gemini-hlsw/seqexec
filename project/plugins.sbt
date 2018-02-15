// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.22")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"          % "sbt-revolver"           % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"      % "sbt-buildinfo"          % "0.7.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"    % "1.3.2")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "1.0.0")

// add and check headers
addSbtPlugin("de.heikoseeberger" % "sbt-header"             % "3.0.1")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"                % "0.9.3")

addSbtPlugin("org.wartremover"   % "sbt-wartremover"        % "2.2.1")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"     % "sbt-scalajs-bundler"    % "0.10.0")

resolvers +=
  Resolver.sonatypeRepo("snapshots")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "0.1.2-SNAPSHOT")

// Avoids a warning message when starting sbt-git
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
