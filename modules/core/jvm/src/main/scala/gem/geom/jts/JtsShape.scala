// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom.jts

import gem.geom.jts.syntax.all._

import gem.geom.Shape
import gem.math.Offset
import org.locationtech.jts.awt.{PointTransformation, ShapeWriter}
import org.locationtech.jts.geom.{Coordinate, Geometry}

import java.awt.geom.Point2D;

/**
 * JVM / JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {

  import JtsShape._

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  override def area: Long =
    g.getArea.round

  /**
   * Converts to the AWT equivalent at 1 arcsec / pixel.  This is specific to
   * the Java implementation.  (The scale should probably be explicitly
   * specified.)
   */
  def toAwt: java.awt.Shape =
    awtWriter.toShape(g)
}

object JtsShape {

  // Converts points to AWT at arcsec / pixel scale.
  private val toArcsec: PointTransformation =
    new PointTransformation {
      def transform(s: Coordinate, d: Point2D): Unit =
        // With AWT canvas, y increases towards the bottom
        d.setLocation(s.x / 1000000.0, - s.y / 1000000.0)
    }

  private val awtWriter: ShapeWriter =
    new ShapeWriter(toArcsec)

}
