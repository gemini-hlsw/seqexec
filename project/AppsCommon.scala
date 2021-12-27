import com.typesafe.sbt.packager.MappingsHelper._
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._
import sbt.Keys._
import sbt.{ Project, Resolver, _ }

/**
 * Define tasks and settings used by application definitions
 */
object AppsCommon {
  lazy val ocsJreDir             = settingKey[File]("Directory where distribution JREs are stored.")
  lazy val applicationConfName   =
    settingKey[String]("Name of the application to lookup the configuration")
  lazy val applicationConfSite   =
    settingKey[DeploymentSite]("Name of the site for the application configuration")
  lazy val downloadConfiguration =
    taskKey[Seq[File]]("Download a configuration file for an application")

  sealed trait LogType
  object LogType {
    case object ConsoleAndFiles extends LogType
    case object Files           extends LogType
  }

  sealed trait DeploymentTarget {
    def subdir: String
  }
  object DeploymentTarget       {
    case object Linux64 extends DeploymentTarget {
      override def subdir: String = "linux"
    }
  }

  sealed trait DeploymentSite {
    def site: String
  }
  object DeploymentSite       {
    case object GS extends DeploymentSite {
      override def site: String = "gs"
    }
    case object GN extends DeploymentSite {
      override def site: String = "gn"
    }
  }

  /**
   * Mappings common to applications, including configuration and logging conf
   */
  lazy val deployedAppMappings = Seq(
    // Don't include the configuration on the jar. Instead we copy it to the conf dir
    Compile / packageBin / mappings ~= { _.filter(!_._1.getName.endsWith(".conf")) },

    // Copy the configuration file
    Universal / packageZipTarball / mappings += {
      val f = (Compile / resourceDirectory).value / "app.conf"
      f -> ("conf/" + f.getName)
    }
  )

  private def embeddedJreSettings(target: DeploymentTarget) = Seq(
    // Put the jre in the tarball
    Universal / mappings ++= {
      val jresDir    = (ThisBuild / ocsJreDir).value
      // Map the location of jre files
      val jreLink    = "JRE64_1.8"
      val linux64Jre = jresDir.toPath.resolve(target.subdir).resolve(jreLink)
      directory(linux64Jre.toFile).map { j =>
        j._1 -> j._2.replace(jreLink, "jre")
      }
    },

    // Make the launcher use the embedded jre
    Universal / javaOptions ++= Seq(
      "-java-home ${app_home}/../jre"
    )
  )

  lazy val embeddedJreSettingsLinux64 = embeddedJreSettings(DeploymentTarget.Linux64)

  /**
   * Settings for meta projects to make them non-publishable
   */
  def preventPublication(p: Project) =
    p.settings(
      publish           := {},
      publishLocal      := {},
      publishArtifact   := false,
      publishTo         := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      packagedArtifacts := Map.empty
    )
}
