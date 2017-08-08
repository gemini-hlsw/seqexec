// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum._
import gem.math.Wavelength

import java.time.Duration

import scalaz._

/**
 * Instrument configuration that is specified for each [[gem.Step Step]].
 * @group Configurations
 */
sealed abstract class DynamicConfig extends Product with Serializable {

  type I <: Instrument with Singleton
  def instrument: I

  /** Obtains the smart gcal search key that corresponds to the instrument
    * configuration, if any. This key can be used to find the matching gcal
    * configuration.
    * @return corresponding smart gcal search key, if any
    */
  def smartGcalKey: Option[DynamicConfig.SmartGcalSearchKey] =
    this match {
      case f2: DynamicConfig.F2        => Some(f2.key)
      case gn: DynamicConfig.GmosNorth => Some(gn.key)
      case gs: DynamicConfig.GmosSouth => Some(gs.key)
      case _                           => None
    }
}

object DynamicConfig {

  type Aux[I0] = DynamicConfig { type I = I0 }

  /** Marker trait for smart gcal search keys used to lookup corresponding gcal
    * configurations.
    */
  sealed trait SmartGcalSearchKey

  /** Marker trait for smart gcal definition keys used to register entries in
    * a smart gcal lookup table.
    */
  sealed trait SmartGcalDefinitionKey

  object SmartGcalKey {

    final case class F2(
      disperser: Option[F2Disperser],
      filter:    F2Filter,
      fpu:       Option[F2Fpu]
    ) extends SmartGcalSearchKey with SmartGcalDefinitionKey

    final case class GmosCommon[D, F, U](
      disperser: Option[D],
      filter:    Option[F],
      fpu:       Option[U],
      xBinning:  GmosXBinning,
      yBinning:  GmosYBinning,
      ampGain:   GmosAmpGain
    )

    type GmosNorthCommon = GmosCommon[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
    type GmosSouthCommon = GmosCommon[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]

    final case class GmosNorthSearch(
      gmos:       GmosNorthCommon,
      wavelength: Option[Wavelength]
    ) extends SmartGcalSearchKey

    final case class GmosSouthSearch(
      gmos:       GmosSouthCommon,
      wavelength: Option[Wavelength]
    ) extends SmartGcalSearchKey

    final case class GmosDefinition[D, F, U](
      gmos:            GmosCommon[D, F, U],
      wavelengthRange: (Wavelength, Wavelength)
    )

    type GmosNorthDefinition = GmosDefinition[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
    type GmosSouthDefinition = GmosDefinition[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]
  }

  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends DynamicConfig {
    type I = I0
  }

  /** @group Constructors */ final case class AcqCam()   extends DynamicConfig.Impl(Instrument.AcqCam)
  /** @group Constructors */ final case class Bhros()    extends DynamicConfig.Impl(Instrument.Bhros)
  /** @group Constructors */ final case class Gnirs()    extends DynamicConfig.Impl(Instrument.Gnirs)
  /** @group Constructors */ final case class Gpi()      extends DynamicConfig.Impl(Instrument.Gpi)
  /** @group Constructors */ final case class Gsaoi()    extends DynamicConfig.Impl(Instrument.Gsaoi)
  /** @group Constructors */ final case class Michelle() extends DynamicConfig.Impl(Instrument.Michelle)
  /** @group Constructors */ final case class Nici()     extends DynamicConfig.Impl(Instrument.Nici)
  /** @group Constructors */ final case class Nifs()     extends DynamicConfig.Impl(Instrument.Nifs)
  /** @group Constructors */ final case class Niri()     extends DynamicConfig.Impl(Instrument.Niri)
  /** @group Constructors */ final case class Phoenix()  extends DynamicConfig.Impl(Instrument.Phoenix)
  /** @group Constructors */ final case class Trecs()    extends DynamicConfig.Impl(Instrument.Trecs)
  /** @group Constructors */ final case class Visitor()  extends DynamicConfig.Impl(Instrument.Visitor)


  import F2Config.F2FpuChoice

  /** @group Constructors */
  final case class F2(
    disperser:     Option[F2Disperser],
    exposureTime:  Duration,
    filter:        F2Filter,
    fpu:           Option[F2FpuChoice],
    lyotWheel:     F2LyotWheel,
    readMode:      F2ReadMode,
    windowCover:   F2WindowCover
  ) extends DynamicConfig.Impl(Instrument.Flamingos2) {

    /** Returns the smart gcal search key for this F2 configuration. */
    def key: SmartGcalKey.F2 =
      SmartGcalKey.F2(disperser, filter, fpu.flatMap(_.toBuiltin))
  }

  object F2 {
    val Default: F2 =
      F2(None, java.time.Duration.ZERO, F2Filter.Open,
         None, F2LyotWheel.F16, F2ReadMode.Bright, F2WindowCover.Close)
  }

  import Gmos._

  /** @group Constructors */
  final case class GmosNorth(
    common:  GmosCommonDynamicConfig,
    grating: Option[GmosGrating[GmosNorthDisperser]],
    filter:  Option[GmosNorthFilter],
    fpu:     Option[GmosCustomMask \/ GmosNorthFpu]
  ) extends DynamicConfig.Impl(Instrument.GmosN) {

    /** Returns the smart gcal search key for this GMOS-N configuration. */
    def key: SmartGcalKey.GmosNorthSearch =
      SmartGcalKey.GmosNorthSearch(
        SmartGcalKey.GmosCommon(
          grating.map(_.disperser),
          filter,
          fpu.flatMap(_.toOption),
          common.ccdReadout.xBinning,
          common.ccdReadout.yBinning,
          common.ccdReadout.ampGain
        ),
        grating.map(_.wavelength)
      )
  }

  object GmosNorth {
    val Default: GmosNorth =
      GmosNorth(GmosCommonDynamicConfig.Default, None, None, None)
  }


  /** @group Constructors */
  final case class GmosSouth(
    common:  GmosCommonDynamicConfig,
    grating: Option[GmosGrating[GmosSouthDisperser]],
    filter:  Option[GmosSouthFilter],
    fpu:     Option[GmosCustomMask \/ GmosSouthFpu]
  ) extends DynamicConfig.Impl(Instrument.GmosS) {

    /** Returns the smart gcal search key for this GMOS-S configuration. */
    def key: SmartGcalKey.GmosSouthSearch =
      SmartGcalKey.GmosSouthSearch(
        SmartGcalKey.GmosCommon(
          grating.map(_.disperser),
          filter,
          fpu.flatMap(_.toOption),
          common.ccdReadout.xBinning,
          common.ccdReadout.yBinning,
          common.ccdReadout.ampGain
        ),
        grating.map(_.wavelength)
      )
  }

  object GmosSouth {
    val Default: GmosSouth =
      GmosSouth(GmosCommonDynamicConfig.Default, None, None, None)
  }

}
