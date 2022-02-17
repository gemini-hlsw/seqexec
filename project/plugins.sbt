addSbtPlugin("edu.gemini" % "sbt-lucuma" % "0.3.9")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

// Support making distributions
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.8")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt" % "sbt-git"    % "1.0.2")
addSbtPlugin("com.dwijnand"     % "sbt-dynver" % "4.1.1")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0")

// Used to find dependencies
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("com.timushev.sbt" % "sbt-updates"          % "0.6.2")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

onLoad in Global := { s => "dependencyUpdates" :: s }
