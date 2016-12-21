import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport._

import Settings.Libraries._

/**
  * Defines the modules used for the Seqexec Engine
  */
trait SeqexecEngineModules extends Common {
  // The variables defined at this level can be referred
  // by the build.sbt files at the module level

  // List all the modules and their inter dependencies
  lazy val edu_gemini_seqexec_server = project
    .in(file("modules/edu.gemini.seqexec.server"))
    .dependsOn(edu_gemini_seqexec_engine, edu_gemini_seqexec_model_JVM)
    .settings(
      libraryDependencies ++=
        Seq(ScalaZStream,
            Argonaut,
            CommonsHttp,
            Squants.value,
            // OCS bundles
            SpModelCore,
            SeqexecOdb,
            POT,
            EpicsACM,
            Knobs
        ) ++ TestLibs.value
    )

  // This should eventually replaced by seqexec_server
  lazy val edu_gemini_seqexec_engine = project
    .in(file("modules/edu.gemini.seqexec.engine"))
    .dependsOn(edu_gemini_seqexec_model_JVM)
    .settings(libraryDependencies ++= Seq(ScalaZStream) ++ TestLibs.value)

  // Unfortunately crossProject doesn't seem to work properly at the module/build.sbt level
  // We have to define the project properties at this level
  lazy val edu_gemini_seqexec_model = crossProject.crossType(CrossType.Pure)
    .in(file("modules/edu.gemini.seqexec.model"))
    .settings(libraryDependencies ++= Seq(BooPickle.value, ScalaZCore.value) ++ TestLibs.value)

  lazy val edu_gemini_seqexec_model_JVM:Project = edu_gemini_seqexec_model.jvm

  lazy val edu_gemini_seqexec_model_JS:Project = edu_gemini_seqexec_model.js
}
