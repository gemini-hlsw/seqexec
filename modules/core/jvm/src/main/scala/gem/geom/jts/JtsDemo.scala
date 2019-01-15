// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom.jts

import gem.geom.jts.interpreter._
import gem.geom.GmosOiwfsProbeArm

import java.awt._
import java.awt.event._

import scala.collection.JavaConverters._

/**
 * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
 */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.AsInstanceOf"))
object JtsDemo extends Frame("JTS Demo") {

  val canvasSize: Int = 800

  val hints: Map[RenderingHints.Key, Object] =
    Map(
      RenderingHints.KEY_ANTIALIASING -> RenderingHints.VALUE_ANTIALIAS_ON,
      RenderingHints.KEY_RENDERING    -> RenderingHints.VALUE_RENDER_QUALITY
    )

  object canvas extends Canvas {
    setBackground(Color.lightGray)
    setSize(canvasSize, canvasSize)

    override def paint(g: Graphics): Unit = {
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHints(hints.asJava)
      g2d.translate(canvasSize/2, canvasSize/2)

      // Cross hair showing center.
      g2d.drawLine(-2, 0, 2, 0)
      g2d.drawLine(0, -2, 0, 2)

      GmosOiwfsProbeArm.shape.eval match {
        case jts: JtsShape => g2d.draw(jts.toAwt)
        case x             => sys.error(s"Whoa unexpected shape type: $x")
      }

    }
  }

  def main(args: Array[String]): Unit = {
    setSize(canvasSize, canvasSize)

    addWindowListener(new WindowAdapter() {
      override def windowClosing(windowEvent: WindowEvent): Unit = {
        System.exit(0)
      }
    })

    add(BorderLayout.CENTER, canvas)

    setVisible(true)
  }
}