// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.enum._
import java.time.Duration

/**
 * Additional type hierarchy over the low-level GMOS enums.
 * @group Instrument-Specific Models
 */
object Gmos {

  final case class GmosNodAndShuffle(
    /*placeholder for now*/
  )

  /** Shared static configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonStaticConfig(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle]
  )

  object GmosCommonStaticConfig {
    val Default: GmosCommonStaticConfig =
      GmosCommonStaticConfig(
        GmosDetector.HAMAMATSU,
        MosPreImaging.IsNotMosPreImaging,
        None
      )
  }

  /** Parameters that determine GMOS CCD readout.
    */
  final case class GmosCcdReadout(
    xBinning:    GmosXBinning,
    yBinning:    GmosYBinning,
    ampCount:    GmosAmpCount,
    ampGain:     GmosAmpGain,
    ampReadMode: GmosAmpReadMode
  )

  object GmosCcdReadout {
    val Default: GmosCcdReadout =
      GmosCcdReadout(
        GmosXBinning.One,
        GmosYBinning.One,
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

  /** Custom mask definition, which is available as an alternative to using a
    * builtin FPU.  Either both these parameters are set or neither are set in a
    * GMOS observation
    */
  final case class GmosCustomMask(
    maskDefinitionFilename: String,
    slitWidth:              GmosCustomSlitWidth
  )

  /** GMOS grating central wavelength.  For now, just a value class wrapper
    * around a BigDecimal.  This should be switched to Fixed / squants?
    */
  final case class GmosCentralWavelength(val nm: BigDecimal) extends AnyVal

  /** GMOS grating configuration, parameterized on the disperser type.  These
    * are grouped because they only apply when using a grating.  That is, all
    * are defined or none or defined in the dynamic config.
    *
    * @tparam D disperser type, expected to be `GmosNorthDisperser` or
    *           `GmosSouthDisperser`
    */
  final case class GmosGrating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: GmosCentralWavelength
  )
}
