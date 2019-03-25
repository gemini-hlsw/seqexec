// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import gem.enum.Site

import cats.implicits._

import java.time.{ Duration, Instant }
import scala.math.Ordering.Implicits._


/** Description of the start/end times for a night according to some criterion.
  * For example, the official observing night begins and ends at 2PM local time
  * while twilight bounded nights start and end according to defined angles of
  * the sun below the horizon.
  */
trait Night {

  /** Location at which the times described by this night are valid. */
  def site: Site

  /** Start instant of the night (inclusive). */
  def start: Instant

  /** End instant of the night (exclusive). */
  def end: Instant

  /** Duration of the night. */
  def duration: Duration =
    Duration.between(start, end)

  /** Returns `true` if the night includes the given `Instant`. */
  def includes(time: Instant): Boolean =
    (start <= time) && (time < end)
}
