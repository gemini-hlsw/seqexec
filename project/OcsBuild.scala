import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._
import sbt.Keys._
import sbt._
import Settings.Libraries._

object OcsBuild extends Build {

  // The variables defined at this level can be referred
  // by the build.sbt files at the module level

  // List all the modules and their inter dependencies
  lazy val edu_gemini_seqexec_server = project
    .in(file("modules/edu.gemini.seqexec.server"))
    .dependsOn(edu_gemini_seqexec_model_JVM)

  // Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
  // We have to define the project properties at this level
  lazy val edu_gemini_seqexec_model = crossProject.crossType(CrossType.Pure)
    .in(file("modules/edu.gemini.seqexec.model"))
    .settings(
      libraryDependencies ++= UPickle.value +: TestLibs.value
    )
    .jsSettings(
      scalaJSUseRhino := false
    )

  lazy val edu_gemini_seqexec_model_JVM:Project = edu_gemini_seqexec_model.jvm

  lazy val edu_gemini_seqexec_model_JS:Project = edu_gemini_seqexec_model.js

  // Only the root is needed for composite web application modules
  lazy val edu_gemini_seqexec_web = project
    .in(file("modules/edu.gemini.seqexec.web"))

}