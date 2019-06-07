// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats._
import cats.implicits._
import cats.effect.IO
import gsp.math.optics.Format
import java.time.{ Instant, ZonedDateTime }
import java.time.ZoneOffset.UTC
import java.time.temporal.ChronoUnit.MICROS


/** Timestamp wraps a `java.util.Instant` that is truncated to microsecond
  * resolution.  This allows Timestamps to roundtrip to/from the database
  * where only microsecond resolution is supported.  In addition the min
  * and max instants that are supported are determined by the postgres limit for
  * timestamps.
  *
  * @param toInstant
  */
sealed abstract case class Timestamp(toInstant: Instant) {

  import Timestamp.{ MaxInstant, MinInstant }

  // Guaranteed by construction, but verified here.
  assert(!toInstant.isBefore(MinInstant))
  assert(!toInstant.isAfter(MaxInstant))
  assert((toInstant.getNano % 1000L) === 0)

  /** Gets the number of seconds from the Java epoch of 1970-01-01T00:00:00Z. */
  def epochSecond: Long =
    toInstant.getEpochSecond

  /** Gets the number of microseconds after the start of the second returned
    * by `epochSecond`.
    */
  def µs: Long =
    toInstant.getNano / 1000L

  /** Converts this instant to the number of milliseconds from the epoch of
    * 1970-01-01T00:00:00Z.
    */
  def toEpochMilli: Long =
    toInstant.toEpochMilli

  /** Creates an updated instance of Timestamp by applying the given
    * function to its wrapped Instant.  The computed Instant is truncated to
    * microsecond precision and validated.
    */
  private def mod(f: Instant => Instant): Option[Timestamp] =
    Timestamp.fromInstant(f(toInstant))

  def plusMillis(millisToAdd: Long): Option[Timestamp] =
    mod(_.plusMillis(millisToAdd))

  def plusMicros(microsToAdd: Long): Option[Timestamp] =
    mod(_.plusNanos(microsToAdd * 1000))

  def plusSeconds(secondsToAdd: Long): Option[Timestamp] =
    mod(_.plusSeconds(secondsToAdd))

  override def toString: String =
    toInstant.toString
}

object Timestamp {

  private val MinInstant: Instant =
    ZonedDateTime.of( -4712, 1, 1, 0, 0, 0, 0, UTC).toInstant

  private val MaxInstant: Instant =
    ZonedDateTime.of(294275, 12, 31, 23, 59, 59, 999999000, UTC).toInstant

  /** Minimum time that can be stored in a postgres timestamp. */
  val Min: Timestamp =
    unsafeFromInstant(MinInstant)

  /** Maximum time that can be stored in a postgres timestamp. */
  val Max: Timestamp =
    unsafeFromInstant(MaxInstant)

  /** `Instant.EPOCH` transformed to `Timestamp`. */
  val Epoch: Timestamp =
    unsafeFromInstant(Instant.EPOCH)

  /** Creates a Timestamp from the given Instant, assuring that the time
    * value recorded has a round number of microseconds and that it is within
    * the range supported by postgres.
    *
    * @group Constructors
    */
  def fromInstant(i: Instant): Option[Timestamp] = {
    val iʹ = i.truncatedTo(MICROS)
    if (iʹ.isBefore(MinInstant) || iʹ.isAfter(MaxInstant)) None
    else Some(new Timestamp(iʹ) {})
  }

  /** Creates a Timestamp from the given Instant if possible, throwing an
    * exception if the time is out of the valid range `Min` <= time <= `Max`.
    *
    * @group Constructors
    */
  def unsafeFromInstant(i: Instant): Timestamp =
    fromInstant(i).getOrElse(sys.error(s"$i out of Timestamp range"))

  val instant: Format[Instant, Timestamp] =
    Format[Instant, Timestamp](fromInstant, _.toInstant)

  /** Creates a Timestamp representing the current time, truncated to the
    * last integral number of microseconds.
    *
    * @group Constructors
    */
  def now: IO[Timestamp] =
    IO {
      unsafeFromInstant(Instant.now())
    }

  /** Creates a Timestamp representing the current time using milliseconds
    * from the Java time epoch.
    *
    * @group Constructors
    */
  def ofEpochMilli(epochMilli: Long): Option[Timestamp] =
    fromInstant(Instant.ofEpochMilli(epochMilli))

  implicit val OrderingTimestamp: Ordering[Timestamp] =
    Ordering.by(_.toInstant)

  implicit val OrderTimestamp: Order[Timestamp] =
    Order.fromOrdering

  implicit val ShowTimestamp: Show[Timestamp] =
    Show.fromToString
}
