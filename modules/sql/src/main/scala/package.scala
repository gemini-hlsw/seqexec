// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import doobie._
import java.time.{ Duration, ZoneId }

package object sql {
  import Angle._

  implicit val DurationMeta: Meta[Duration] =
    Meta[Long].xmap(Duration.ofMillis, _.toMillis)

  implicit val ArcsecondsMeta: Meta[Arcseconds] =
    Meta[BigDecimal].xmap(Arcseconds.fromArcsecs, _.toArcsecs)

  implicit val DegreesMeta: Meta[Degrees] =
    Meta[BigDecimal].xmap(Degrees.fromDegrees, _.toDegrees)

  implicit val WavelengthNmMeta: Meta[Wavelength.Nm] =
    Meta[BigDecimal].xmap(Wavelength.fromNm, _.toBigDecimal)

  implicit val WavelengthUmMeta: Meta[Wavelength.Um] =
    Meta[BigDecimal].xmap(Wavelength.fromUm, _.toBigDecimal)

  implicit val ZoneIdMeta: Meta[ZoneId] =
    Meta[String].xmap(ZoneId.of, _.toString)

  implicit val MagnitudSystemMeta: Meta[MagnitudeSystem] =
    Meta[String].xmap(MagnitudeSystem, _.id)

  implicit val MagnitudeValueMeta: Meta[MagnitudeValue] =
    Meta[BigDecimal].xmap(MagnitudeValue.apply, _.value)

}
