// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.CoAdds
import gem.enum._
import gem.math.Wavelength

import cats.Eq
import cats.implicits._
import java.time.Duration

import monocle._
import monocle.std.either.{ stdLeft, stdRight }
import monocle.std.option.some

/**
 * Instrument configuration that is specified for each [[gem.Step Step]].
 * @group Configurations
 */
sealed trait DynamicConfig {

  /** Obtains the smart gcal search key that corresponds to the instrument
    * configuration, if any. This key can be used to find the matching gcal
    * configuration.
    * @return corresponding smart gcal search key, if any
    */
  def smartGcalKey(s: StaticConfig): Option[SmartGcalSearchKey] =
    (this, s) match {
      case (d: DynamicConfig.Flamingos2, _)                => Some(d.key)
      case (d: DynamicConfig.GmosN, _)                     => Some(d.key)
      case (d: DynamicConfig.GmosS, _)                     => Some(d.key)
      case (d: DynamicConfig.Gnirs, s: StaticConfig.Gnirs) => Some(d.key(s))
      case _                                               => None
    }

  def toStep(b: Step.Base): Step

}

object DynamicConfig {

  /**
   * Dynamic configuration for the acquisition camera.
   * @group Constructors
   */
  final case class AcqCam() extends DynamicConfig {
    def toStep(b: Step.Base): Step.AcqCam =
      Step.AcqCam(this, b)
  }

  /**
   * Dynamic configuration for bHROS (retired).
   * @group Constructors
   */
  final case class Bhros() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Bhros =
      Step.Bhros(this, b)
  }

  /**
   * Dynamic configuration for GHOST.
   * @group Constructors
   */
  final case class Ghost() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Ghost =
      Step.Ghost(this, b)
  }

  /**
   * Dynamic configuration for GPI.
   * @group Constructors
   */
  final case class Gpi() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Gpi =
      Step.Gpi(this, b)
  }

  /**
   * Dynamic configuration for GSAOI.
   * @group Constructors
   */
  final case class Gsaoi() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Gsaoi =
      Step.Gsaoi(this, b)
  }

  /**
   * Dynamic configuration for Michelle (retired).
   * @group Constructors
   */
  final case class Michelle() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Michelle =
      Step.Michelle(this, b)
  }

  /**
   * Dynamic configuration for NICI (retired).
   * @group Constructors
   */
  final case class Nici() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Nici =
      Step.Nici(this, b)
  }

  /**
   * Dynamic configuration for NIFS.
   * @group Constructors
   */
  final case class Nifs() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Nifs =
      Step.Nifs(this, b)
  }

  /**
   * Dynamic configuration for NIRI.
   * @group Constructors
   */
  final case class Niri() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Niri =
      Step.Niri(this, b)
  }

  /**
   * Dynamic configuration for Phoenix.
   * @group Constructors
   */
  final case class Phoenix() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Phoenix =
      Step.Phoenix(this, b)
  }

  /**
   * Dynamic configuration for T-ReCS (retired).
   * @group Constructors
   */
  final case class Trecs() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Trecs =
      Step.Trecs(this, b)
  }

  /**
   * Dynamic configuration for visitor instruments.
   * @group Constructors
   */
  final case class Visitor() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Visitor =
      Step.Visitor(this, b)
  }


  /**
   * Dynamic configuration for FLAMINGOS-2.
   * @group Constructors
   */
  final case class Flamingos2(
    disperser:     Option[F2Disperser],
    exposureTime:  Duration,
    filter:        F2Filter,
    fpu:           Option[F2Config.F2FpuChoice],
    lyotWheel:     F2LyotWheel,
    readMode:      F2ReadMode,
    windowCover:   F2WindowCover
  ) extends DynamicConfig {

    def toStep(b: Step.Base): Step.Flamingos2 =
      Step.Flamingos2(this, b)

    /** Returns the smart gcal search key for this Flamingos2 configuration. */
    def key: SmartGcalKey.Flamingos2 =
      SmartGcalKey.Flamingos2(disperser, filter, fpu.flatMap(_.toBuiltin))

  }
  object Flamingos2 {
    val Default: Flamingos2 =
      Flamingos2(None, java.time.Duration.ZERO, F2Filter.Open,
         None, F2LyotWheel.F16, F2ReadMode.Bright, F2WindowCover.Close)
  }

  /**
   * Dynamic configuration for GMOS-N.
   * @group Constructors
   */
  final case class GmosN(
    common:  GmosConfig.GmosCommonDynamicConfig,
    grating: Option[GmosConfig.GmosGrating[GmosNorthDisperser]],
    filter:  Option[GmosNorthFilter],
    fpu:     Option[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]]
  ) extends DynamicConfig {

    def toStep(b: Step.Base): Step.GmosN =
      Step.GmosN(this, b)

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

  object GmosN extends GmosNOptics {

    val Default: GmosN =
      GmosN(GmosConfig.GmosCommonDynamicConfig.Default, None, None, None)

    implicit val EqualGmosN: Eq[GmosN] =
      Eq.by(g => (g.common, g.grating, g.filter, g.fpu))

  }

  trait GmosNOptics {

    /** @group Optics */
    val common: Lens[GmosN, GmosConfig.GmosCommonDynamicConfig] =
      Lens[GmosN, GmosConfig.GmosCommonDynamicConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val grating: Lens[GmosN, Option[GmosConfig.GmosGrating[GmosNorthDisperser]]] =
      Lens[GmosN, Option[GmosConfig.GmosGrating[GmosNorthDisperser]]](_.grating)(a => _.copy(grating = a))

    /** @group Optics */
    val filter: Lens[GmosN, Option[GmosNorthFilter]] =
      Lens[GmosN, Option[GmosNorthFilter]](_.filter)(a => _.copy(filter = a))

    /** @group Optics */
    val fpu: Lens[GmosN, Option[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]]] =
      Lens[GmosN, Option[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]]](_.fpu)(a => _.copy(fpu = a))

    private val someGrating: Optional[GmosN, GmosConfig.GmosGrating[GmosNorthDisperser]] =
      grating composePrism some

    /** @group Optics */
    val disperser: Optional[GmosN, GmosNorthDisperser] =
      someGrating composeLens GmosConfig.GmosGrating.disperser[GmosNorthDisperser]

    /** @group Optics */
    val wavelength: Optional[GmosN, Wavelength] =
      someGrating composeLens GmosConfig.GmosGrating.wavelength

    private val someFpu: Optional[GmosN, Either[GmosConfig.GmosCustomMask, GmosNorthFpu]] =
      fpu composePrism some

    /** @group Optics */
    val builtinFpu: Optional[GmosN, GmosNorthFpu] =
      someFpu composePrism stdRight

    /** @group Optics */
    val customMask: Optional[GmosN, GmosConfig.GmosCustomMask] =
      someFpu composePrism stdLeft

  }

  /**
   * Dynamic configuration for GMOS-S.
   * @group Constructors
   */
  final case class GmosS(
    common:  GmosConfig.GmosCommonDynamicConfig,
    grating: Option[GmosConfig.GmosGrating[GmosSouthDisperser]],
    filter:  Option[GmosSouthFilter],
    fpu:     Option[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]]
  ) extends DynamicConfig {

    def toStep(b: Step.Base): Step.GmosS =
      Step.GmosS(this, b)

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

  object GmosS extends GmosSOptics {

    val Default: GmosS =
      GmosS(GmosConfig.GmosCommonDynamicConfig.Default, None, None, None)

    implicit val EqualGmosS: Eq[GmosS] =
      Eq.by(g => (g.common, g.grating, g.filter, g.fpu))

  }

  trait GmosSOptics {

    /** @group Optics */
    val common: Lens[GmosS, GmosConfig.GmosCommonDynamicConfig] =
      Lens[GmosS, GmosConfig.GmosCommonDynamicConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val grating: Lens[GmosS, Option[GmosConfig.GmosGrating[GmosSouthDisperser]]] =
      Lens[GmosS, Option[GmosConfig.GmosGrating[GmosSouthDisperser]]](_.grating)(a => _.copy(grating = a))

    /** @group Optics */
    val filter: Lens[GmosS, Option[GmosSouthFilter]] =
      Lens[GmosS, Option[GmosSouthFilter]](_.filter)(a => _.copy(filter = a))

    /** @group Optics */
    val fpu: Lens[GmosS, Option[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]]] =
      Lens[GmosS, Option[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]]](_.fpu)(a => _.copy(fpu = a))

    private val someGrating: Optional[GmosS, GmosConfig.GmosGrating[GmosSouthDisperser]] =
      grating composePrism some

    /** @group Optics */
    val disperser: Optional[GmosS, GmosSouthDisperser] =
      someGrating composeLens GmosConfig.GmosGrating.disperser[GmosSouthDisperser]

    /** @group Optics */
    val wavelength: Optional[GmosS, Wavelength] =
      someGrating composeLens GmosConfig.GmosGrating.wavelength

    private val someFpu: Optional[GmosS, Either[GmosConfig.GmosCustomMask, GmosSouthFpu]] =
      fpu composePrism some

    /** @group Optics */
    val builtinFpu: Optional[GmosS, GmosSouthFpu] =
      someFpu composePrism stdRight

    /** @group Optics */
    val customMask: Optional[GmosS, GmosConfig.GmosCustomMask] =
      someFpu composePrism stdLeft

  }


  /**
   * Dynamic configuration for GNIRS.
   * @group Constructors
   */
  final case class Gnirs(
    acquisitionMirror: GnirsAcquisitionMirror,
    camera:            GnirsCamera,
    coadds:            CoAdds,
    decker:            GnirsDecker,
    disperser:         GnirsDisperser,
    exposureTime:      Duration,
    filter:            GnirsFilter,
    fpu:               Either[GnirsFpuOther, GnirsFpuSlit],
    prism:             GnirsPrism,
    readMode:          GnirsReadMode,
    wavelength:        Wavelength
  ) extends DynamicConfig {

    def toStep(b: Step.Base): Step.Gnirs =
      Step.Gnirs(this, b)

    /** Returns the smart gcal search key for this GNIRS configuration. */
    def key(s: StaticConfig.Gnirs): SmartGcalKey.GnirsSearch =
      SmartGcalKey.GnirsSearch(
        SmartGcalKey.Gnirs(
          acquisitionMirror,
          camera.pixelScale,
          disperser,
          fpu,
          prism,
          s.wellDepth
        ),
        wavelength
      )

  }
  object Gnirs {
    val Default: Gnirs = Gnirs(
      GnirsAcquisitionMirror.Out,
      GnirsCamera.ShortBlue,
      CoAdds.One,
      GnirsDecker.Acquisition,
      GnirsDisperser.D32,
      java.time.Duration.ofSeconds(17),
      GnirsFilter.Order5,
      Right(GnirsFpuSlit.LongSlit_0_30),
      GnirsPrism.Mirror,
      GnirsReadMode.Bright,
      Wavelength.fromAngstroms.unsafeGet(22000)
    )
  }

  implicit val EqDynamicConfig: Eq[DynamicConfig] =
    Eq.fromUniversalEquals // TODO: ensure this is ok

}
