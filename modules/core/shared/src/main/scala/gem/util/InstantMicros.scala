// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats._

import java.time.Instant
import java.time.temporal.ChronoUnit.MICROS


/** InstantMicros wraps a `java.util.Instant` that is truncated to microsecond
  * resolution.  This allows InstantMicros to roundtrip to/from the database
  * where timestamps support only microsecond resolution.
  *
  * @param toInstant
  */
final class InstantMicros private (val toInstant: Instant) extends AnyVal {

  /** Gets the number of seconds from the Java epoch of 1970-01-01T00:00:00Z. */
  def epochSecond: Long =
    toInstant.getEpochSecond

  /** Gets the number of microseconds after the start of the second returned
    * by `epochSecond`.
    */
  def µs: Long =
    toInstant.getNano / 1000l

  /** Converts this instant to the number of milliseconds from the epoch of
    * 1970-01-01T00:00:00Z.
    */
  def toEpochMilli: Long =
    toInstant.toEpochMilli

  /** Creates an updated instance of InstantMicros by applying the given
    * function to its wrapped Instant.
    */
  def mod(f: Instant => Instant): InstantMicros =
    InstantMicros.truncate(f(toInstant))

  def plusMillis(millisToAdd: Long): InstantMicros =
    mod(_.plusMillis(millisToAdd))

  def plusMicros(microsToAdd: Long): InstantMicros =
    mod(_.plusNanos(microsToAdd * 1000))

  def plusSeconds(secondsToAdd: Long): InstantMicros =
    mod(_.plusSeconds(secondsToAdd))

  override def toString: String =
    toInstant.toString
}

object InstantMicros {

  val Min: InstantMicros = InstantMicros.truncate(Instant.MIN)
  val Max: InstantMicros = InstantMicros.truncate(Instant.MAX)


  /** Creates an InstantMicro from the given Instant, assuring that the time
    * value recorded has a round number of microseconds.
    *
    * @group Constructors
    */
  def truncate(i: Instant): InstantMicros =
    new InstantMicros(i.truncatedTo(MICROS))

  /** Creates an InstantMicro representing the current time, truncated to the
    * last integral number of microseconds.
    *
    * @group Constructors
    */
  def now(): InstantMicros =
    truncate(Instant.now())

  /** Creates an InstantMicro representing the current time using milliseconds
    * from the Java time epoch.
    *
    * @group Constructors
    */
  def ofEpochMilli(epochMilli: Long): InstantMicros =
    new InstantMicros(Instant.ofEpochMilli(epochMilli))

  implicit val OrderingInstantMicros: Ordering[InstantMicros] =
    Ordering.by(_.toInstant)

  implicit val OrderInstantMicros: Order[InstantMicros] =
    Order.fromOrdering

  implicit val ShowInstantMicros: Show[InstantMicros] =
    Show.fromToString
}
