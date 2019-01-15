// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom

/**
 * Interprets a `ShapeExpression` to a `Shape`.
 */
trait ShapeInterpreter {
  def interpret(e: ShapeExpression): Shape
}
