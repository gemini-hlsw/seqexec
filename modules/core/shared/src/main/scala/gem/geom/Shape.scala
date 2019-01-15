// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom

import gem.math.Offset

/**
 * Shape description usable for testing point inclusion and calculating the
 * area.  Point inclusion is used to determine, for instance, guide star
 * reachability for a probe range.  Comparative area is usable when calculating
 * which guide star options produce the minimum vignetting (probe arm shadow
 * intersected with science area).
 */
trait Shape {

  /**
   * Determines whether the given position is contained in the shape.
   */
  def contains(o: Offset): Boolean

  /**
   * Area of the shape. The value is in microarcseconds angular separation^2
   * but the absolute value is less useful than comparing across multiple
   * shapes (to determine minimum vignetting).
   */
  def area: Long

}
