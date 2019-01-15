// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom.jts.syntax

import gem.geom.jts.Jts.geometryFactory

import angle._
import offset._

import gem.math.Offset

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry}
import org.locationtech.jts.util.GeometricShapeFactory

// Syntax used in the JVM/ JTS implementation only.

final class OffsetOps(val self: Offset) extends AnyVal {

  def µas: (Long, Long) =
    (self.p.toAngle.µas, self.q.toAngle.µas)

  def coordinate: Coordinate = {
    val (p, q) = µas
    // Offset p is flipped around the y axis so it increases to the left
    new Coordinate(-p.toDouble, q.toDouble)
  }

  def point: Geometry =
    geometryFactory.createPoint(coordinate)

  def envelope(that: Offset): Envelope =
    new Envelope(coordinate, that.coordinate)

  def shapeFactory(that: Offset): GeometricShapeFactory = {
    val f  = new GeometricShapeFactory(geometryFactory)
    f.setEnvelope(envelope(that))
    f
  }

}

trait ToOffsetOps {
  implicit def ToOffsetOps(o: Offset): OffsetOps = new OffsetOps(o)
}

object offset extends ToOffsetOps
