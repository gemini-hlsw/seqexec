import Settings.Libraries._
import sbt.Keys._
import sbt.{file, Compile, ModuleID, project}

/**
  * Define tasks and settings used by module definitions
  */
object Common {
  lazy val commonSettings = Seq(
    scalaOrganization := "org.typelevel",
    scalaVersion := Settings.LibraryVersions.scalaVersion,
    scalacOptions ++= Settings.Definitions.scalacOptions,
    scalacOptions in (Compile, console) ~= (_.filterNot(Set(
      "-Xfatal-warnings",
      "-Ywarn-unused:imports"
    ))),

    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value) ++ TestLibs.value
  )

  lazy val commonJSSettings = commonSettings ++ Seq(
    // These settings allow to use TLS with scala.js
    // Remove the dependency on the scalajs-compiler
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler")
  )

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

}
