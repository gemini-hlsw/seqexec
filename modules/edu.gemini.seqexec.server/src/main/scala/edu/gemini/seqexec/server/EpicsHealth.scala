package edu.gemini.seqexec.server

/**
  * Created by jluhrs on 7/18/17.
  */
sealed trait EpicsHealth

object EpicsHealth {
  object Good extends EpicsHealth
  object Bad extends EpicsHealth
  implicit def fromInt(v: Int): EpicsHealth = if(v == 0) Good else Bad
}
