// Gives support for Scala.js compilation
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.13")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray" % "sbt-revolver" % "0.8.0")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

// Support making distributions
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M7")
