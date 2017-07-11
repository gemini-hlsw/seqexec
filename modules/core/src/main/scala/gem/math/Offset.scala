// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

final case class Offset(p: Offset.P, q: Offset.Q)
object Offset {

  val Zero: Offset =
    Offset(P.Zero, Q.Zero)

  final case class P(toAngle: Angle)
  object P {
    val Zero: P =
      P(Angle.Angle0)
  }

  final case class Q(toAngle: Angle)
  object Q {
    val Zero: Q =
      Q(Angle.Angle0)
  }

}
