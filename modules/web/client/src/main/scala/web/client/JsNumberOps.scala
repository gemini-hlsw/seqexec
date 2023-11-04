// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.Eq
import cats.syntax.all._
import japgolly.scalajs.react.facade.JsNumber

object JsNumberOps {
  implicit val jsNumberEq: Eq[JsNumber] = Eq.instance { (a, b) =>
    (a: Any, b: Any) match {
      case (a: Double, b: Double) => a === b
      case (a: Int, b: Int)       => a === b
      case (a: Long, b: Long)     => a === b
      case (a: Float, b: Float)   => a === b
      case (a: Short, b: Short)   => a === b
      case (a: Byte, b: Byte)     => a === b
      case _                      => false
    }
  }

  implicit final class JsNumberOps(val d: JsNumber) extends AnyVal {
    // Some uglies for js union types
    def toDouble: Double =
      (d: Any) match {
        case d: Float  => d.toDouble
        case d: Double => d
        case d: Byte   => d.toDouble
        case d: Short  => d.toDouble
        case d: Int    => d.toDouble
        case _         => sys.error("Unsupported type")
      }

    // Some uglies for js union types
    def toInt: Int =
      (d: Any) match {
        case d: Float  => d.toInt
        case d: Double => d.toInt
        case d: Byte   => d.toInt
        case d: Short  => d.toInt
        case d: Int    => d
        case _         => sys.error("Unsupported type")
      }
  }

}
