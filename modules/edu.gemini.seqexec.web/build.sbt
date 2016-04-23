import Settings.Libraries._

name := "edu.gemini.seqexec.web"

// Root web project
lazy val edu_gemini_seqexec_web = project.in(file("."))
  .aggregate(edu_gemini_seqexec_web_server, edu_gemini_seqexec_web_client, edu_gemini_seqexec_web_shared_JS, edu_gemini_seqexec_web_shared_JVM)

lazy val commonSettings = Seq(
  // Common libraries
  libraryDependencies ++= Seq(ScalaZCore.value, UPickle.value) ++ TestLibs.value
)

// a special crossProject for configuring a JS/JVM/shared structure
lazy val edu_gemini_seqexec_web_shared = (crossProject.crossType(CrossType.Pure) in file("edu.gemini.seqexec.web.shared"))
  .settings(commonSettings: _*)
  .jvmSettings(
  )
  .jsSettings(
    scalaJSUseRhino := false
  )

lazy val edu_gemini_seqexec_web_shared_JVM = edu_gemini_seqexec_web_shared.jvm

lazy val edu_gemini_seqexec_web_shared_JS = edu_gemini_seqexec_web_shared.js

// Client side project using Scala.js
lazy val edu_gemini_seqexec_web_client = project.in(file("edu.gemini.seqexec.web.client"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    // Skip tests in module, Rhino doesn't play nice with jquery
    test := {},
    // Write the generated js to the filename seqexec.js
    artifactPath in (Compile, fastOptJS) := (resourceManaged in Compile).value / "seqexec.js",
    // JS dependencies from webjars
    jsDependencies ++= Seq(
      "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
      "org.webjars.bower" % "react"       % LibraryVersions.reactJS     / "react-dom.js"         minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
      "org.webjars"       % "jquery"      % LibraryVersions.jQuery      / "jquery.js"            minified "jquery.min.js",
      "org.webjars"       % "Semantic-UI" % LibraryVersions.semanticUI  / "semantic.js"          minified "semantic.min.js" dependsOn "jquery.js"
    ),
    // Build a js dependencies file
    skip in packageJSDependencies := false,
    // Put the jsdeps file on a place reachable for the server
    crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
    libraryDependencies ++= Seq(
      JQuery.value,
      ScalaCSS.value,
      ScalaJSDom.value
    ) ++ ReactScalaJS.value ++ Diode.value
  )
  .settings(
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(name, version),
    buildInfoPackage := "edu.gemini.seqexec.web.client"
  )
  .dependsOn(edu_gemini_seqexec_web_shared_JS % "compile->compile;test->test", edu_gemini_seqexec_model_JS)

// This function allows triggered compilation to run only when scala files changes
// It lets change static files freely
def includeInTrigger(f: java.io.File): Boolean =
  f.isFile && {
    val name = f.getName.toLowerCase
    name.endsWith(".scala") || name.endsWith(".js")
  }

// Project for the server side application
lazy val edu_gemini_seqexec_web_server = project.in(file("edu.gemini.seqexec.web.server"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= ScalaZCore.value +: (Http4s ++ Play),

    // Settings to optimize the use of sbt-revolver
    
    // Allows to read the generated JS on client
    resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
    // Lets the backend to read the .map file for js
    resources in Compile += (fastOptJS in (edu_gemini_seqexec_web_client, Compile)).value.map((x: sbt.File) => new File(x.getAbsolutePath + ".map")).data,
    // Lets the server read the jsdeps file
    (managedResources in Compile) += (artifactPath in(edu_gemini_seqexec_web_client, Compile, packageJSDependencies)).value,
    // Support stopping the running server
    //mainClass in reStart := Some("edu.gemini.seqexec.web.server.play.WebServerLauncher"),
    mainClass in reStart := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
    // do a fastOptJS on reStart
    reStart <<= reStart dependsOn (fastOptJS in (edu_gemini_seqexec_web_client, Compile)),
    // This settings makes reStart to rebuild if a scala.js file changes on the client
    watchSources ++= (watchSources in edu_gemini_seqexec_web_client).value,
    // On recompilation only consider changes to .scala and .js files
    watchSources ~= { t:Seq[java.io.File] => {t.filter(includeInTrigger)} }

  )
  .dependsOn(edu_gemini_seqexec_web_shared_JVM, edu_gemini_seqexec_server)

