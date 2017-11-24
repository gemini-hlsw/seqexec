// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.client

import org.scalajs.dom
import org.scalajs.dom.html
import scala.math

trait utils {
  type Canvas = html.Canvas
  type Ctx2D = dom.CanvasRenderingContext2D

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def textWidth(text: String, font: String): Int = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[Ctx2D]
    ctx.font = font
    val metrics = ctx.measureText(text)
    math.round(metrics.width.toFloat)
  }
}

object utils extends utils
