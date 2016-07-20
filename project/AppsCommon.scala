import sbt.Keys._
import sbt.{Def, IO, Project, Resolver, _}

/**
  * Define tasks and settings used by application definitions
  */
trait AppsCommon {
  lazy val ocsJreDir = settingKey[File]("Directory where distribution JREs are stored.")

  sealed trait LogType
  object LogType {
    case object ConsoleAndFiles extends LogType
    case object Files extends LogType
  }

  /**
    * Task to generate a logging configuration file from a template
    */
  def generateLoggingConfigTask(logType: LogType) = Def.task {
    val loggingConfDestName = "logging.properties"
    val loggingConfSrcName = logType match {
      case LogType.Files           => "logging.files.template"
      case LogType.ConsoleAndFiles => "logging.console_files.template"
    }

    val loggingTemplate = (baseDirectory in ThisBuild).value / "project" / "resources" / loggingConfSrcName
    val config = IO.read(loggingTemplate).replace("{{app.name}}", name.value)
    target.value.mkdirs()
    val destFile = target.value / loggingConfDestName
    println(s"Generating configuration of type $logType to ${destFile.getPath}")
    IO.write(destFile, config)
    destFile
  }

  /**
    * Settings for meta projects to make them non-publishable
    */
  def preventPublication(p: Project) =
    p.settings(
      publish := {},
      publishLocal := {},
      publishArtifact := false,
      publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      packagedArtifacts := Map.empty)
}
