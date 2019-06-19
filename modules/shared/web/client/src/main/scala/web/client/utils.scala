// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.CatsReact._
import org.scalajs.dom
import org.scalajs.dom.html
import react.common.implicits._

trait utils {
  type Canvas = html.Canvas
  type Ctx2D  = dom.CanvasRenderingContext2D

  def textWidth(text: String, font: String): Double = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx    = canvas.getContext("2d").asInstanceOf[Ctx2D]
    ctx.font = font
    val metrics = ctx.measureText(text)
    metrics.width
  }

  def tableTextWidth(text: String): Double =
    textWidth(text, "bold 14px sans-serif")

}

object utils extends utils {
  implicit val reuse: Reusability[JsNumber] = Reusability.byEq
}
