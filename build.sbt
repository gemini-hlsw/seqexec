
name := Settings.Definitions.name

scalaVersion in ThisBuild := Settings.LibraryVersions.scala

scalacOptions in ThisBuild ++= Settings.Definitions.scalacOptions

organization in Global := "edu.gemini.ocs"

// Gemini repository
resolvers in ThisBuild += "Gemini Repository" at "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"

def preventPublication(p: Project) =
  p.settings(
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    packagedArtifacts := Map.empty)

val generateLoggingConfigTask = Def.task {
      val loggingTemplate = (baseDirectory in ThisBuild).value / "project" / "resources" / "logging.properties"
      val config = IO.read(loggingTemplate).replace("{{app.name}}", name.value)
      resourceManaged.value.mkdirs()
      IO.write(resourceManaged.value / loggingTemplate.getName, config)
      Seq(loggingTemplate)
    }

lazy val seqexec = preventPublication(project.in(file("app/seqexec")))
  .dependsOn(edu_gemini_seqexec_web_server)
  .aggregate(edu_gemini_seqexec_web_server)
  .enablePlugins(UniversalPlugin)
  .enablePlugins(LinuxPlugin)
  .enablePlugins(RpmPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .settings(
    name in Universal := "seqexec",
    mainClass in Compile := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
    //mainClass in Compile := Some("edu.gemini.seqexec.web.server.play.WebServerLauncher"),

    // RPM properties
    rpmVendor := "Gemini",
    version in Rpm := {
      (version in ThisBuild).value.replace("-SNAPSHOT", "")
    },
    // Altering permissions for configs
    linuxPackageMappings := {
        val mappings = linuxPackageMappings.value
        // Changing the group for all configs
        mappings map {
            case linuxPackage if linuxPackage.fileData.config equals "true" =>
                // altering the group
                val newFileData = linuxPackage.fileData.copy(
                    group = "appdocs"
                )
                // altering the LinuxPackageMapping
                linuxPackage.copy(
                    fileData = newFileData
                )
            case linuxPackage => linuxPackage
        }
    },

    // The production optimized files will go into the seqexec jar
    // Run full opt js on the javascript. They will be placed on the "seqexec" jar
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client, Compile)).value,
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client_cli, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client_cli, Compile)).value,

    //  Generate a custom logging.properties for the application
    resourceGenerators in Compile += generateLoggingConfigTask.taskValue,

    // Put the jar files in the lib dir
    mappings in Universal <+= (packageBin in Compile) map { jar =>
      jar -> ("lib/" + jar.getName)
    },

    // Launch options
    javaOptions in Universal ++= Seq(
      // -J params will be added as jvm parameters
      "-J-Xmx512m",
      "-J-Xms256m",

      // others will be added as app parameters
      // TODO Define how to configure applications
      "prod" // Run in production mode.
    )
  )
