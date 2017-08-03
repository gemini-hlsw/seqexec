// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import scalaz.{ Order, Show }
import scalaz.std.anyVal._

/**
 * Celestial longitude, measured eastward along the celestial equator from the vernal equinox to the
 * hour circle of the point in question. This class is a simple newtype wrapper for an [[HourAngle]].
 * @see The helpful [[https://en.wikipedia.org/wiki/Right_ascension Wikipedia]] article.
 * @param toHourAngle the underlying hour angle
 */
final case class RightAscension(toHourAngle: HourAngle) {

  /**
   * Offset this `RightAscension` by the given hour angle.
   * @group Operations
   */
  def offset(ha: HourAngle): RightAscension =
    RightAscension(toHourAngle + ha)

  /**
   * Flip this `RightAscension` 180°, as might be required when offsetting coordinates causes the
   * associated declination to cross a pole.
   * @group Operations
   */
  def flip: RightAscension =
    RightAscension(toHourAngle.flip)

  /** Forget that this [[RightAscension]] wraps an [[HourAngle]]. */
  def toAngle: Angle =
    toHourAngle

  override def toString =
    s"RA(${toHourAngle.toHMS})"

}

object RightAscension {

  /**
   * Construct a `RightAscension` from an [[HourAngle]]. Alias for `apply`.
   * @group Constructors
   */
  def fromHourAngle(ha: HourAngle): RightAscension =
    apply(ha)

  /**
   * The `RightAscension` at zero degrees.
   * @group Constructors
   */
  val Zero: RightAscension =
    RightAscension(HourAngle.HourAngle0)

  /**
   * Unlike arbitrary angles, it is common to order right asensions starting at zero hours.
   * @group Typeclass Instances
   */
  implicit val RightAscensionOrder: Order[RightAscension] =
    Order.orderBy(_.toHourAngle.toMicroseconds)

  /* @group Typeclass Instances */
  implicit val RightAscensionShow: Show[RightAscension] =
    Show.showA

}
