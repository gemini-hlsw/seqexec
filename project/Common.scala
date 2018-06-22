import Settings.Libraries._
import sbt.Keys._
import sbt._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import wartremover.WartRemover.autoImport._
import com.timushev.sbt.updates.UpdatesPlugin.autoImport._
import org.flywaydb.sbt.FlywayPlugin.autoImport._

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
      Wart.ImplicitConversion, // we know what we're doing
      Wart.ImplicitParameter   // we do finally tagless
    )

  lazy val commonSettings = Seq(
    // Workaround for https://github.com/sbt/sbt/issues/4109
    initialCommands += "jline.TerminalFactory.get.init\n",

    scalaVersion                            := Settings.LibraryVersions.scalaVersion,
    scalacOptions                          ++= Settings.Definitions.scalacOptions,
    scalacOptions in (Compile, console)     ~= (_.filterNot(Set(
      "-Xfatal-warnings",
      "-Ywarn-unused:imports"
    ))),
    scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-skip-packages", "scalaz",
      "-doc-title", "Gem",
      "-doc-version", version.value
    ),
    // These sbt-header settings can't be set in ThisBuild for some reason
    headerMappings                          := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
    headerLicense                           := Some(HeaderLicense.Custom(
      """|Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
         |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
         |""".stripMargin
    )),
    // Common libraries
    libraryDependencies                    ++= TestLibs.value,
    // Wartremover in compile and test (not in Console)
    wartremoverErrors in (Compile, compile) := gemWarts,
    wartremoverErrors in (Test,    compile) := gemWarts,
    // Don't build javadoc when we're packaging the docker image.
    mappings in (Compile, packageDoc)       := Seq(),
    sources in (Compile,doc)                := Seq.empty,
    // Temporary, needed for decline 0.4.0-M1
    resolvers += Resolver.jcenterRepo,

    // We don't care to see updates about the scala language itself
    dependencyUpdatesFilter -= moduleFilter(name = "scala-library"),
    dependencyUpdatesFilter -= moduleFilter(name = "scala-reflect"),
    // Don't worry about stale deps pulled in by scala-js
    dependencyUpdatesFilter -= moduleFilter(organization = "org.eclipse.jetty"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "gem.test.Tags.RequiresNetwork"), // by default, ignore network tests
    // Don't worry about monocle versions that start with the same prefix.
    dependencyUpdatesFilter -= moduleFilter(
      organization = "com.github.julien-truffaut",
      revision = sbt.io.GlobFilter(Settings.LibraryVersions.monocleVersion.replace("-cats", "*"))
    )
  )

  lazy val commonJSSettings = commonSettings ++ Seq(
    scalacOptions ~= (_.filterNot(Set(
      // Allows a certain reduction on the ouput js file
      "-Xcheckinit"
    ))),
    // activate the ScalaJS defined annotation by default
    scalacOptions       += "-P:scalajs:sjsDefinedByDefault"
  )

  lazy val flywaySettings = Seq(
    flywayUrl  := "jdbc:postgresql:gem",
    flywayUser := "postgres",
    flywayLocations := Seq(
      s"filesystem:${baseDirectory.value}/src/main/resources/db/migration"
    )
  )

}
