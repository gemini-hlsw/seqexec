// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import gem.geom.ShapeExpression
import gem.geom.ShapeExpression._

import gem.math.{Angle, Offset}

final class ShapeExpressionOps(val self: ShapeExpression) extends AnyVal {

  def difference(that: ShapeExpression): ShapeExpression =
    Difference(self, that)

  def intersection(that: ShapeExpression): ShapeExpression =
    Intersection(self, that)

  def rotate(a: Angle): ShapeExpression =
    Rotate(self, a)

  def translate(o: Offset): ShapeExpression =
    Translate(self, o)

  def union(that: ShapeExpression): ShapeExpression =
    Union(self, that)

  def -(that: ShapeExpression): ShapeExpression =
    difference(that)

  def ∩(that: ShapeExpression): ShapeExpression =
    intersection(that)

  def ⟲(a: Angle): ShapeExpression =
    rotate(a)

  def ↗(o: Offset): ShapeExpression =
    translate(o)

  def ∪(that: ShapeExpression): ShapeExpression =
    union(that)

}

trait ToShapeExpressionOps {
  implicit def ToShapeExpressionOps(e: ShapeExpression): ShapeExpressionOps =
    new ShapeExpressionOps(e)
}

final class ShapeExpressionCompanionOps(val self: ShapeExpression.type) extends AnyVal {

  // Simplified constructors

  private def offset(o: (Angle, Angle)): Offset =
    Offset(Offset.P(o._1), Offset.Q(o._2))

  def ellipse(a: (Angle, Angle), b: (Angle, Angle)): ShapeExpression =
    Ellipse(offset(a), offset(b))

  def polygon(os: (Angle, Angle)*): ShapeExpression =
    Polygon(os.toList.map(offset))

  def rectangle(a: (Angle, Angle), b: (Angle, Angle)): ShapeExpression =
    Rectangle(offset(a), offset(b))

}

trait ToShapeExpressionCompanionOps {
  implicit def ToShapeExpressionCompanionOps(c: ShapeExpression.type): ShapeExpressionCompanionOps =
    new ShapeExpressionCompanionOps(c)
}

object shapeexpression extends ToShapeExpressionOps with ToShapeExpressionCompanionOps