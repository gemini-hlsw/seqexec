// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.{ Angle, HourAngle }

trait AngleMeta {

  private def signedBigDecimal(d: Int): Meta[Angle] =
    Meta[java.math.BigDecimal]
      .timap[Angle](
        b => Angle.fromMicroarcseconds(b.movePointRight(d).longValue))(
        a => new java.math.BigDecimal(Angle.signedMicroarcseconds.get(a)).movePointLeft(d)
      )

  // Angle mapping to signed arcseconds via NUMERIC. NOT implicit. We're mapping a type that
  // is six orders of magnitude more precise than the database column, so we will shift
  // the decimal pure back and forth.
  val AngleMetaAsSignedArcseconds: Meta[Angle] =
    signedBigDecimal(6)

  val AngleMetaAsSignedMilliarcseconds: Meta[Angle] =
    signedBigDecimal(3)

  val AngleMetaAsMicroarcseconds: Meta[Angle] =
    Meta[Long].timap[Angle](Angle.fromMicroarcseconds)(_.toMicroarcseconds)

  val HourAngleMetaAsMicroseconds: Meta[HourAngle] =
    Meta[Long].timap[HourAngle](HourAngle.fromMicroseconds)(_.toMicroseconds)

}
object AngleMeta extends AngleMeta
