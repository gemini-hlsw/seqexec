// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math.Wavelength

trait WavelengthMeta {

  // Wavelength maps to an integer in angstroms
  implicit val WavelengthMeta: Meta[Wavelength] =
    Meta[Int].xmap(Wavelength.unsafeFromAngstroms, _.toAngstroms)

}
object WavelengthMeta extends WavelengthMeta
