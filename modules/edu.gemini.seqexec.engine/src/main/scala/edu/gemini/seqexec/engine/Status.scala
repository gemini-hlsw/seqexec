package edu.gemini.seqexec.engine

/**
  * Flag to indicate whether the global execution is `Running` or `Waiting`.
  */
sealed trait Status

object Status {
  case object Waiting   extends Status
  case object Completed extends Status
  case object Running   extends Status
}
