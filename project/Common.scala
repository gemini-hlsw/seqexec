import Settings.Libraries._
import sbt.Keys._
import sbt.{file, ModuleID, project}

/**
  * Define tasks and settings used by module definitions
  */
object Common {
  lazy val commonSettings = Seq(
    scalaOrganization := "org.typelevel",
    scalaVersion := Settings.LibraryVersions.scalaVersion,
    scalacOptions ++= Settings.Definitions.scalacOptions,

    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value) ++ TestLibs.value,
    // This is needed to support the TLS compiler and scala.js at the same time
    libraryDependencies ~= { (libDeps: Seq[ModuleID]) =>
      libDeps.filterNot(dep => dep.name == "scalajs-compiler")
    }
  )

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

}
