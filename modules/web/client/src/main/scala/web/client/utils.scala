// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import japgolly.scalajs.react.raw.JsNumber
import org.scalajs.dom
import org.scalajs.dom.html

trait utils {
  type Canvas = html.Canvas
  type Ctx2D  = dom.CanvasRenderingContext2D

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def textWidth(text: String, font: String): Int = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx    = canvas.getContext("2d").asInstanceOf[Ctx2D]
    ctx.font = font
    val metrics = ctx.measureText(text)
    math.round(metrics.width.toFloat)
  }

  def tableTextWidth(text: String): Int = textWidth(text, "bold 14px sans-serif")

}

object utils extends utils {

  implicit class JsNumberOps(val d: JsNumber) extends AnyVal {

    // Some uglies for js union types
    def toDouble: Double = (d: Any) match {
      case d: Float  => d.toDouble
      case d: Double => d
      case d: Byte   => d.toDouble
      case d: Short  => d.toDouble
      case d: Int    => d.toDouble
    }
  }

}
