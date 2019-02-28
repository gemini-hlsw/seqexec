// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math.Wavelength

trait WavelengthMeta {
  import PrismMeta._

  // Wavelength maps to an integer in pm
  implicit val WavelengthMeta: Meta[Wavelength] =
    Wavelength.fromPicometers.asMeta

}
object WavelengthMeta extends WavelengthMeta
