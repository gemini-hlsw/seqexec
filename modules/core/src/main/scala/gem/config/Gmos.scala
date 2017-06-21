// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.enum._

import java.time.Duration

/** Additional hierarchy over the low-level GMOS enums. */
object Gmos {

  final case class GmosNodAndShuffle(
    /*placeholder for now*/
  )

  /** Shared static configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonStaticConfig(
    detector:      GmosDetector,
    mosPreImaging: Boolean,
    nodAndShuffle: Option[GmosNodAndShuffle]
  )

  object GmosCommonStaticConfig {
    val Default: GmosCommonStaticConfig =
      GmosCommonStaticConfig(
        GmosDetector.HAMAMATSU,
        false,
        None
      )
  }

  final case class GmosCcdReadout(
    xBin:     GmosBinning,
    yBin:     GmosBinning,
    count:    GmosAmpCount,
    gain:     GmosAmpGain,
    readMode: GmosAmpReadMode
  )

  object GmosCcdReadout {
    val Default: GmosCcdReadout =
      GmosCcdReadout(
        GmosBinning.One,
        GmosBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )
  }

  /** Shared dynamic configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonDynamicConfig(
    ccdReadout:   GmosCcdReadout,
    dtaxOffset:   GmosDtax,
    exposureTime: Duration
  )

  object GmosCommonDynamicConfig {
    val Default: GmosCommonDynamicConfig =
      GmosCommonDynamicConfig(
        GmosCcdReadout.Default,
        GmosDtax.Zero,
        Duration.ofSeconds(300)
      )
  }

  final case class GmosCustomMask(
    maskDefinitionFilename: String,
    slitWidth:              GmosCustomSlitWidth
  )

  /** GMOS grating central wavelength.  For now, just a value class wrapper
    * around a Double.  This should be switched to Fixed / squants?
    */
  final class GmosCentralWavelength(val nm: Double) extends AnyVal

  /** GMOS-N grating configuration.  These are grouped because they only apply
    * using a grating.
    */
  final case class GmosNorthGrating(
    disperser:  GmosNorthDisperser,
    order:      GmosDisperserOrder,
    wavelength: GmosCentralWavelength
  )

  /** GMOS-S grating configuration.  These are grouped because they only apply
    * using a grating.
    */
  final case class GmosSouthGrating(
    disperser:  GmosNorthDisperser,
    order:      GmosDisperserOrder,
    wavelength: GmosCentralWavelength
  )
}
