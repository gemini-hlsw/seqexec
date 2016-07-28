import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.MappingsHelper._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging.autoImport._
import com.typesafe.sbt.packager.archetypes.JavaServerAppPackaging
import com.typesafe.sbt.packager.linux.LinuxPlugin
import com.typesafe.sbt.packager.linux.LinuxPlugin.autoImport._
import com.typesafe.sbt.packager.rpm.RpmPlugin
import com.typesafe.sbt.packager.rpm.RpmPlugin.autoImport._
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import sbt.Keys._
import sbt._

/**
  * Application level wodules for the Seqexec
  */
trait SeqexecApps extends AppsCommon with SeqexecWebModules {
  /**
    * Common settings for the Seqexec instances
    */
  lazy val seqexecCommonSettings = Seq(
    // Main class for launching
    mainClass in Compile := Some("edu.gemini.seqexec.web.server.http4s.WebServerLauncher"),
    // This is important to keep the file generation order correctly
    parallelExecution in Universal := false,
    // Run full opt js on the javascript. They will be placed on the "seqexec" jar
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client, Compile)).value,
    resources in Compile += (fullOptJS in (edu_gemini_seqexec_web_client_cli, Compile)).value.data,
    resources in Compile += (packageMinifiedJSDependencies in (edu_gemini_seqexec_web_client_cli, Compile)).value,
    // Name of the launch script
    executableScriptName := "seqexec-server",
    // No javadocs
    mappings in (Compile, packageDoc) := Seq(),
    // Don't create launchers for Windows
    makeBatScript := None,
    // Specify a different name for the config file
    bashScriptConfigLocation := Some("${app_home}/../conf/launcher.args"),
    // Launch options
    javaOptions in Universal ++= Seq(
      // -J params will be added as jvm parameters
      "-J-Xmx512m",
      "-J-Xms256m"
    )
  )

  /**
    * Project for the seqexec server app for development
    */
  lazy val seqexec_server = preventPublication(project.in(file("app/seqexec-server")))
    .dependsOn(edu_gemini_seqexec_web_server)
    .aggregate(edu_gemini_seqexec_web_server)
    .enablePlugins(JavaServerAppPackaging)
    .settings(seqexecCommonSettings: _*)
    .settings(
      description := "Seqexec server for local testing",

      // Generate a custom logging.properties for the application
      // For staging the log uses files and console
      mappings in Universal += {
        val f = generateLoggingConfigTask(LogType.ConsoleAndFiles).value
        f -> ("conf/" + f.getName)
      },

      // Put the jar files in the lib dir
      mappings in Universal <+= (packageBin in Compile) map { jar =>
        jar -> ("lib/" + jar.getName)
      }
    )

  /**
    * Project for the seqexec server app for testing on Linux 64
    */
  lazy val seqexec_server_test_l64 = preventPublication(project.in(file("app/seqexec-server-test-l64")))
    .dependsOn(edu_gemini_seqexec_web_server)
    .aggregate(edu_gemini_seqexec_web_server)
    .enablePlugins(LinuxPlugin, RpmPlugin)
    .enablePlugins(JavaServerAppPackaging)
    .settings(seqexecCommonSettings: _*)
    .settings(
      description := "Seqexec server test deployment on linux 64",

      // RPM properties
      rpmVendor := "Gemini",
      rpmLicense := Some("BSD-3"),
      rpmGroup := Some("Gemini"),
      rpmChangelogFile := None,
      packageDescription in Rpm := "Seqexec Server",
      rpmPrefix in Rpm := Some("/gemsoft/opt"),
      packageName in Rpm := "seqexec-server",
      // User/Group for execution
      daemonUser in Linux := "telops",
      daemonGroup in Linux := "telops",
      // This lets us build RPMs from snapshot versions
      version in Rpm := {
        (version in ThisBuild).value.replace("-SNAPSHOT", "")
      },
      // The distribution uses only log files, no console
      mappings in Universal in packageZipTarball += {
        val f = generateLoggingConfigTask(LogType.Files).value
        f -> ("conf/" + f.getName)
      },

      // Don't include the configuration on the jar. Instead we copy it to the conf dir
      mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".conf")) },

      // Copy the configuration file
      mappings in Universal in packageZipTarball += {
        val f = (resourceDirectory in Compile).value / "app.conf"
        f -> ("conf/" + f.getName)
      },

      // Put the jre in the tarball
      mappings in Universal ++= {
        val jresDir = (ocsJreDir in ThisBuild).value
        // Map the location of jre files
        val jreLink = "JRE64_1.8"
        val linux64Jre = jresDir.toPath.resolve("linux").resolve(jreLink)
        directory(linux64Jre.toFile).map { j =>
          j._1 -> j._2.replace(jreLink, "jre")
        }
      },

      // Make the launcher use the embedded jre
      javaOptions in Universal ++= Seq(
        "-java-home ${app_home}/../jre"
      ),

      // Put the jre on the RPM
      linuxPackageMappings in Rpm += {
        val jresDir = (ocsJreDir in ThisBuild).value
        // RPM are of interest only for linux 64 bit
        val linux64Jre = jresDir.toPath.resolve("linux").resolve("JRE64_1.8")
        packageDirectoryAndContentsMapping((linux64Jre.toFile, (rpmPrefix in Rpm).value.map(_ + "").getOrElse("")))
      }
    ).dependsOn(seqexec_server)
}
