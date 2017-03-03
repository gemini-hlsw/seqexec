package edu.gemini.seqexec.engine

/**
  * A Seqexec resource represents any system that can be only used by one single agent.
  *
  */
sealed trait Resource

object Resource {

  case object GMOS extends Resource
  case object F2 extends Resource
  case object P1 extends Resource
  case object OI extends Resource
  case object Mount extends Resource
  case object ScienceFold extends Resource

}
