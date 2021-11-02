import Settings.Libraries._
import sbt.Keys._
import sbt._
import com.timushev.sbt.updates.UpdatesPlugin.autoImport._

/**
 * Define tasks and settings used by module definitions
 */
object Common {
  lazy val commonSettings = Seq(
    // Workaround for https://github.com/sbt/sbt/issues/4109
    initialCommands += "jline.TerminalFactory.get.init\n",
    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-skip-packages",
      "scalaz",
      "-doc-title",
      "Gem",
      "-doc-version",
      version.value
    ),
    // Common libraries
    libraryDependencies += TestLibs.value,
    // Don't build javadoc when we're packaging the docker image.
    Compile / packageDoc / mappings := Seq(),
    Compile / doc / sources         := Seq.empty,
    // We don't care to see updates about the scala language itself
    dependencyUpdatesFilter -= moduleFilter(name = "scala-library"),
    dependencyUpdatesFilter -= moduleFilter(name = "scala-reflect"),
    // Don't worry about stale deps pulled in by scala-js
    dependencyUpdatesFilter -= moduleFilter(organization = "org.eclipse.jetty"),
    // Don't worry about old ocs related dependencies
    dependencyUpdatesFilter -= moduleFilter(organization = "dom4j"),
    dependencyUpdatesFilter -= moduleFilter(organization = "net.sf.opencsv"),
    dependencyUpdatesFilter -= moduleFilter(organization = "commons-httpclient"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest,
                                         "-l",
                                         "gem.test.Tags.RequiresNetwork"
    ), // by default, ignore network tests
    // Don't worry about monocle versions that start with the same prefix.
    dependencyUpdatesFilter -= moduleFilter(
      organization = "com.github.julien-truffaut",
      revision = sbt.io.GlobFilter(Settings.LibraryVersions.monocleVersion.replace("-cats", "*"))
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

}
