// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom.jts

import syntax.all._

import gem.geom.ShapeExpression.{Difference, Ellipse, Intersection, Polygon, Rectangle, Rotate, Translate, Union}
import gem.geom.{Shape, ShapeExpression, ShapeInterpreter}

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.util.AffineTransformation

/**
 * JVM-specific JTS shape interpreter.
 */
object JtsShapeInterpreter extends ShapeInterpreter {

  private def toGeometry(e: ShapeExpression): Geometry =
    e match {
      // Constructors
      case Ellipse(a, b)      => a.shapeFactory(b).createEllipse
      case Polygon(os)        => Jts.geometryFactory.createPolygon(os.map(_.coordinate).toArray)
      case Rectangle(a, b)    => a.shapeFactory(b).createRectangle

      // Combinations
      case Difference(a, b)   => toGeometry(a).difference(toGeometry(b))
      case Intersection(a, b) => toGeometry(a).intersection(toGeometry(b))
      case Union(a, b)        => toGeometry(a).union(toGeometry(b))

      // Transformations
      case Rotate(e, a)       =>
        AffineTransformation
          .rotationInstance(a.toDoubleDegrees)
          .transform(toGeometry(e))

      case Translate(e, o)    =>
        val c = o.coordinate
        AffineTransformation
          .translationInstance(c.x, c.y)
          .transform(toGeometry(e))
    }

  override def interpret(e: ShapeExpression): Shape =
    JtsShape(toGeometry(e))
}


object interpreter {

  implicit val value: ShapeInterpreter =
    JtsShapeInterpreter

}

