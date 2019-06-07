// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.Wavelength
import gsp.math.syntax.prism._

/**
 * Meta definitions for various wavelength units.  These are *not* implicit
 * because we need different definitions in distinct contexts.
 */
trait WavelengthMeta {
  import PrismMeta._

  // Wavelength mapped to pm via an integer.
  val WavelengthMetaAsPicometers: Meta[Wavelength] =
    Wavelength.fromPicometers.asMeta

  private def bigDecimal(d: Int): Meta[Wavelength] =
    Meta[java.math.BigDecimal]
      .timap[Wavelength](
        b => Wavelength.fromPicometers.unsafeGet(b.movePointRight(d).intValue))(
        w => new java.math.BigDecimal(Wavelength.fromPicometers.reverseGet(w)).movePointLeft(d)
      )

  /**
   * Wavelength mapped to Å via NUMERIC. Integral Å is 10^2 less precise than
   * pm.
   */
  val WavelengthMetaAsAngstroms: Meta[Wavelength] =
    bigDecimal(2)

  /**
   * Wavelength mapped to nm via NUMERIC. Integral nm is 10^3 less precise than
   * pm.
   */
  val WavelengthMetaAsNanometers: Meta[Wavelength] =
    bigDecimal(3)

  /**
   * Wavelength mapped to μm via NUMERIC. Integral μm is 10^6 less precise than
   * pm.
   */
  val WavelengthMetaAsMicrometers: Meta[Wavelength] =
    bigDecimal(6)

}

object WavelengthMeta extends WavelengthMeta
