import sbt._

/**
 * Application settings
 */
object Settings {
  object Definitions {
    /** The name of the application */
    val name = "ocs3"

    /** Top level version */
    val version = "2016001.1.1"

    /** Options for the scala compiler */
    val scalacOptions = Seq(
      "-unchecked",
      "-deprecation",
      "-encoding", "UTF-8", // Keep on the same line
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-target:jvm-1.8",
      "-Xlint",
      "-Xlint:-stars-align"
    )
  }

  /** global dependency versions */
  object LibraryVersions {
    val scala        = "2.11.8"
    val scalaDom     = "0.9.0"
    val scalajsReact = "0.10.4"
    val scalaCSS     = "0.4.0"
    val scalaZ       = "7.1.6"

    val ocsVersion   = "2016001.1.1"
  }

  /** Global libraries */
  object Libraries {
    val ScalaZCore       = "org.scalaz" %% "scalaz-core" % LibraryVersions.scalaZ
    val ScalaZConcurrent = "org.scalaz" %% "scalaz-concurrent" % LibraryVersions.scalaZ
  }
}
