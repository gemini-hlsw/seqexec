package edu.gemini.seqexec.server

/**
 * Created by jluhrs on 7/30/15.
 */
sealed trait ConfigDelta
case class Same[T](value: T) extends ConfigDelta
case class Change[T](oldValue: T, newValue: T) extends ConfigDelta
