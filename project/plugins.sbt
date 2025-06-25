addSbtPlugin("edu.gemini" % "sbt-lucuma-app" % "0.10.13")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

// Support making distributions
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.1")

// Built the version out of git
//addSbtPlugin("com.github.sbt" % "sbt-git"    % "2.0.0")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0")

// Used to find dependencies
addDependencyTreePlugin

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.4")

libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
