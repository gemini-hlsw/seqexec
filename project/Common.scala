import Settings.Libraries._
import sbt.Keys._
import sbt.{file, project}

/**
  * Define tasks and settings used by module definitions
  */
trait Common {
  lazy val commonSettings = Seq(
    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value, UPickle.value, BooPickle.value) ++ TestLibs.value
  )

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

  lazy val edu_gemini_web_server_common = project
    .in(file("modules/edu.gemini.web.server.common"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(ScalaZConcurrent) ++ Http4s
    )

}
