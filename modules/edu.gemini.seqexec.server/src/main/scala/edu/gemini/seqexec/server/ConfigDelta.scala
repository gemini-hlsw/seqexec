package edu.gemini.seqexec.server

/**
 * Created by jluhrs on 7/30/15.
 */
sealed trait ConfigDelta[T]
final case class Same[T](value: T) extends ConfigDelta[T]
final case class Change[T](oldValue: T, newValue: T) extends ConfigDelta[T]

object ConfigDelta {
  def compare[T](v0: T, v1: T) =
    if (v0.equals(v1)) Same(v0)
    else          Change(v0, v1)
}
