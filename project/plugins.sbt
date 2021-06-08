addSbtPlugin("edu.gemini"         % "sbt-lucuma"               % "0.3.7")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.6.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

// sbt revolver lets launching applications from the sbt console
addSbtPlugin("io.spray"           % "sbt-revolver"             % "0.9.1")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.10.0")

// Support making distributions
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.8.1")

// Check the style with scalastyle
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0")

// Built the version out of git
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.1")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"               % "4.1.1")

// Use NPM modules rather than webjars
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.20.0")

// Used to find dependencies
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph"     % "0.9.2")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.5.3")

addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.2")

onLoad in Global := { s => "dependencyUpdates" :: s }
