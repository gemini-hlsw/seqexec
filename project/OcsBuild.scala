import sbt._

object OcsBuild extends Build {

  // The variables defined at this level can be referred
  // by the build.sbt files at the module level

  // List all the modules and their inter dependencies
  lazy val edu_gemini_seqexec_server = project
    .in(file("modules/edu.gemini.seqexec.server"))
    .dependsOn(edu_gemini_seqexec_server_model)

  lazy val edu_gemini_seqexec_server_model = project
    .in(file("modules/edu.gemini.seqexec.model"))

  // Only the root is needed for composite web application modules
  lazy val edu_gemini_seqexec_web = project
    .in(file("modules/edu.gemini.seqexec.web"))

}