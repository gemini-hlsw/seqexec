// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

/** Minimal time classes to support reading from the enum
  * tables.
  */
sealed trait FiniteDuration extends Product with Serializable {
  def toMillis: Long

}

object FiniteDuration {
  final case class Seconds(toMillis: Long) extends FiniteDuration
  final case class Milliseconds(toMillis: Long) extends FiniteDuration

  def fromSeconds(bd: Long): FiniteDuration.Seconds =
    Seconds(bd)

  def fromMilliseconds(bd: Long): FiniteDuration.Milliseconds =
    Milliseconds(bd)
}
