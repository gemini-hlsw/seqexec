import Settings.Libraries.{BooPickle, ScalaZCore, TestLibs}
import sbt.Keys._

/**
  * Define tasks and settings used by module definitions
  */
trait Common {
  lazy val commonSettings = Seq(
    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value, BooPickle.value) ++ TestLibs.value
  )

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }
}
