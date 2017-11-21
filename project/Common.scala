import Settings.Libraries._
import Settings.PluginVersions
import sbt.Keys._
import sbt._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import wartremover.WartRemover.autoImport._

/**
  * Define tasks and settings used by module definitions
  */
object Common {
  lazy val gemWarts =
    Warts.allBut(
      Wart.Any,                // false positives
      Wart.Nothing,            // false positives
      Wart.Null,               // false positives
      Wart.Product,            // false positives
      Wart.Serializable,       // false positives
      Wart.Recursion,          // false positives
      Wart.ImplicitConversion  // we know what we're doing
    )

  lazy val commonSettings = Seq(
    scalaOrganization := "org.typelevel",
    scalaVersion := Settings.LibraryVersions.scalaVersion,
    scalacOptions ++= Settings.Definitions.scalacOptions,
    scalacOptions in (Compile, console) ~= (_.filterNot(Set(
      "-Xfatal-warnings",
      "-Ywarn-unused:imports"
    ))),
    // These sbt-header settings can't be set in ThisBuild for some reason
    headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.CppStyleLineComment),
    headerLicense  := Some(HeaderLicense.Custom(
      """|Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
         |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
         |""".stripMargin
    )),

    // Common libraries
    libraryDependencies ++= Seq(ScalaZCore.value) ++ TestLibs.value,
    // Wartremover in compile and test (not in Console)
    wartremoverErrors in (Compile, compile) := gemWarts,
    wartremoverErrors in (Test,    compile) := gemWarts
  )

  lazy val commonJSSettings = commonSettings ++ Seq(
    // These settings allow to use TLS with scala.js
    // Remove the dependency on the scalajs-compiler
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler"),
    scalacOptions += "-P:scalajs:sjsDefinedByDefault"
  )

  // This function allows triggered compilation to run only when scala files changes
  // It lets change static files freely
  def includeInTrigger(f: java.io.File): Boolean =
    f.isFile && {
      val name = f.getName.toLowerCase
      name.endsWith(".scala") || name.endsWith(".js")
    }

}
