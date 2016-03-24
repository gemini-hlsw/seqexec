import sbt._

object OcsBuild extends Build {

  // We prefer to list the modules in here, as the Scala files can be used to define inter module dependencies
  // at the sbt level per module

  // List all the modules
  lazy val edu_gemini_seqexec_server = project.in(file("modules/edu.gemini.seqexec.server"))

  // Only the root is needed
  lazy val edu_gemini_seqexec_web = project.in(file("modules/edu.gemini.seqexec.web"))

}