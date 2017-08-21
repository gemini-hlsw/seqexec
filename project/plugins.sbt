// Gives support for Scala.js compilation
val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.19")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"          % "sbt-revolver"           % "0.8.0")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"      % "sbt-buildinfo"          % "0.6.1")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"    % "1.2.0-M7")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "0.8.0")

// add and check headers
addSbtPlugin("de.heikoseeberger" % "sbt-header"             % "2.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")

// Avoids a warning message when starting sbt-git
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
