// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom

import gem.math.Angle

import gem.syntax.shapeexpression._

// WIP: would need to be moved out of this package and is missing the
// calculation for particular offset position impact on the arm rotation

/**
 * Description of the GMOS OIWFS probe arm geometry.
 */
object GmosOiwfsProbeArm {
  val PickoffArmLength: Angle      = 358460.mas
  val PickoffMirrorSize: Angle     =  20000.mas
  val ProbeArmLength: Angle        = PickoffArmLength - (PickoffMirrorSize / 2)
  val ProbeArmTaperedWidth: Angle  =  15000.mas
  val ProbeArmTaperedLength: Angle = 180000.mas

  private val arm: ShapeExpression = {
    val hm  = PickoffMirrorSize    / 2
    val htw = ProbeArmTaperedWidth / 2

    val p0 = (-hm,                           htw  )
    val p1 = (p0._1 - ProbeArmTaperedLength, hm   )
    val p2 = (p0._1 - ProbeArmLength,        p1._2)
    val p3 = (p2._1,                        -hm   )
    val p4 = (p1._1,                         p3._2)
    val p5 = (p0._1,                        -htw  )

    ShapeExpression.polygon(p0, p1, p2, p3, p4, p5, p0)
  }

  private val pickoff: ShapeExpression = {
    val s = PickoffMirrorSize / 2
    ShapeExpression.rectangle((s, s), (s - PickoffMirrorSize, s - PickoffMirrorSize))
  }

  /**
   * Description of the GMOS OIWFS probe arm with the pickoff mirror centered
   * at the base position.
   */
  val shape: ShapeExpression =
    arm ∪ pickoff

  // Syntax to simplify the implementation.

  private implicit class IntOps(val self: Int) extends AnyVal {
    def mas: Angle =
      Angle.signedMicroarcseconds.reverseGet(self.toLong * 1000)
  }

  private implicit class AngleOps(val self: Angle) extends AnyVal {
    def /(d: Int): Angle =
      Angle.fromMicroarcseconds(self.toMicroarcseconds / d)
  }

}
