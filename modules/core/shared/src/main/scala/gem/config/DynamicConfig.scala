// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.CoAdds
import gem.enum._
import gem.instances.time._
import gsp.math.Wavelength

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

  object AcqCam {
    implicit val EqAcqCam: Eq[AcqCam] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for bHROS (retired).
   * @group Constructors
   */
  final case class Bhros() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Bhros =
      Step.Bhros(this, b)
  }

  object Bhros {
    implicit val EqBhros: Eq[Bhros] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for GHOST.
   * @group Constructors
   */
  final case class Ghost() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Ghost =
      Step.Ghost(this, b)
  }

  object Ghost {
    implicit val EqGhost: Eq[Ghost] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for GPI.
   * @group Constructors
   */
  final case class Gpi() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Gpi =
      Step.Gpi(this, b)
  }

  object Gpi {
    implicit val EqGpi: Eq[Gpi] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for GSAOI.
   * @group Constructors
   */
  final case class Gsaoi() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Gsaoi =
      Step.Gsaoi(this, b)
  }

  object Gsaoi {
    implicit val EqGsaoi: Eq[Gsaoi] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for Michelle (retired).
   * @group Constructors
   */
  final case class Michelle() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Michelle =
      Step.Michelle(this, b)
  }

  object Michelle {
    implicit val EqMichelle: Eq[Michelle] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for NICI (retired).
   * @group Constructors
   */
  final case class Nici() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Nici =
      Step.Nici(this, b)
  }

  object Nici {
    implicit val EqNici: Eq[Nici] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for NIFS.
   * @group Constructors
   */
  final case class Nifs() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Nifs =
      Step.Nifs(this, b)
  }

  object Nifs {
    implicit val EqNifs: Eq[Nifs] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for NIRI.
   * @group Constructors
   */
  final case class Niri() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Niri =
      Step.Niri(this, b)
  }

  object Niri {
    implicit val EqNiri: Eq[Niri] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for Phoenix.
   * @group Constructors
   */
  final case class Phoenix() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Phoenix =
      Step.Phoenix(this, b)
  }

  object Phoenix {
    implicit val EqPhoenix: Eq[Phoenix] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for T-ReCS (retired).
   * @group Constructors
   */
  final case class Trecs() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Trecs =
      Step.Trecs(this, b)
  }

  object Trecs {
    implicit val EqTrecs: Eq[Trecs] =
      Eq.allEqual
  }

  /**
   * Dynamic configuration for visitor instruments.
   * @group Constructors
   */
  final case class Visitor() extends DynamicConfig {
    def toStep(b: Step.Base): Step.Visitor =
      Step.Visitor(this, b)
  }

  object Visitor {
    implicit val EqVisitor: Eq[Visitor] =
      Eq.allEqual
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

    implicit val EqFlamingos2: Eq[Flamingos2] =
      Eq.by(f => (
        f.disperser,
        f.exposureTime,
        f.filter,
        f.fpu,
        f.lyotWheel,
        f.readMode,
        f.windowCover
      ))
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

    implicit val EqGmosN: Eq[GmosN] =
      Eq.by(g => (g.common, g.grating, g.filter, g.fpu))

  }

  trait GmosNOptics {

    import GmosConfig.{ GmosCcdReadout, GmosCommonDynamicConfig, GmosCustomMask, GmosGrating }

    /** @group Optics */
    val common: Lens[GmosN, GmosConfig.GmosCommonDynamicConfig] =
      Lens[GmosN, GmosCommonDynamicConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val grating: Lens[GmosN, Option[GmosConfig.GmosGrating[GmosNorthDisperser]]] =
      Lens[GmosN, Option[GmosGrating[GmosNorthDisperser]]](_.grating)(a => _.copy(grating = a))

    /** @group Optics */
    val filter: Lens[GmosN, Option[GmosNorthFilter]] =
      Lens[GmosN, Option[GmosNorthFilter]](_.filter)(a => _.copy(filter = a))

    /** @group Optics */
    val fpu: Lens[GmosN, Option[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]]] =
      Lens[GmosN, Option[Either[GmosCustomMask, GmosNorthFpu]]](_.fpu)(a => _.copy(fpu = a))

    /** @group Optics */
    def xBinning: Lens[GmosN, GmosXBinning] =
      common composeLens GmosCommonDynamicConfig.ccdReadout composeLens GmosCcdReadout.xBinning

    /** @group Optics */
    def yBinning: Lens[GmosN, GmosYBinning] =
      common composeLens GmosCommonDynamicConfig.ccdReadout composeLens GmosCcdReadout.yBinning

    /** @group Optics */
    def exposureTime: Lens[GmosN, Duration] =
      common composeLens GmosCommonDynamicConfig.exposureTime

    /** @group Optics */
    def roi: Lens[GmosN, GmosRoi] =
      common composeLens GmosCommonDynamicConfig.roi

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

    implicit val EqGmosS: Eq[GmosS] =
      Eq.by(g => (g.common, g.grating, g.filter, g.fpu))

  }

  trait GmosSOptics {

    import GmosConfig.{ GmosCcdReadout, GmosCommonDynamicConfig, GmosCustomMask, GmosGrating }

    /** @group Optics */
    val common: Lens[GmosS, GmosConfig.GmosCommonDynamicConfig] =
      Lens[GmosS, GmosCommonDynamicConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val grating: Lens[GmosS, Option[GmosConfig.GmosGrating[GmosSouthDisperser]]] =
      Lens[GmosS, Option[GmosGrating[GmosSouthDisperser]]](_.grating)(a => _.copy(grating = a))

    /** @group Optics */
    val filter: Lens[GmosS, Option[GmosSouthFilter]] =
      Lens[GmosS, Option[GmosSouthFilter]](_.filter)(a => _.copy(filter = a))

    /** @group Optics */
    val fpu: Lens[GmosS, Option[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]]] =
      Lens[GmosS, Option[Either[GmosCustomMask, GmosSouthFpu]]](_.fpu)(a => _.copy(fpu = a))

    /** @group Optics */
    def xBinning: Lens[GmosS, GmosXBinning] =
      common composeLens GmosCommonDynamicConfig.ccdReadout composeLens GmosCcdReadout.xBinning

    /** @group Optics */
    def yBinning: Lens[GmosS, GmosYBinning] =
      common composeLens GmosCommonDynamicConfig.ccdReadout composeLens GmosCcdReadout.yBinning

    /** @group Optics */
    def exposureTime: Lens[GmosS, Duration] =
      common composeLens GmosCommonDynamicConfig.exposureTime

    /** @group Optics */
    def roi: Lens[GmosS, GmosRoi] =
      common composeLens GmosCommonDynamicConfig.roi

    private val someGrating: Optional[GmosS, GmosGrating[GmosSouthDisperser]] =
      grating composePrism some

    /** @group Optics */
    val disperser: Optional[GmosS, GmosSouthDisperser] =
      someGrating composeLens GmosGrating.disperser[GmosSouthDisperser]

    /** @group Optics */
    val wavelength: Optional[GmosS, Wavelength] =
      someGrating composeLens GmosGrating.wavelength

    private val someFpu: Optional[GmosS, Either[GmosCustomMask, GmosSouthFpu]] =
      fpu composePrism some

    /** @group Optics */
    val builtinFpu: Optional[GmosS, GmosSouthFpu] =
      someFpu composePrism stdRight

    /** @group Optics */
    val customMask: Optional[GmosS, GmosCustomMask] =
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

    implicit val EqGnirs: Eq[Gnirs] =
      Eq.by(g => (
        g.acquisitionMirror,
        g.camera,
        g.coadds,
        g.decker,
        g.disperser,
        g.exposureTime,
        g.filter,
        g.fpu,
        g.prism,
        g.readMode,
        g.wavelength
      ))
  }

  implicit val EqDynamicConfig: Eq[DynamicConfig] =
    Eq.instance {
      case (a: AcqCam,     b: AcqCam    ) => a === b
      case (a: Bhros,      b: Bhros     ) => a === b
      case (a: Flamingos2, b: Flamingos2) => a === b
      case (a: Ghost,      b: Ghost     ) => a === b
      case (a: Gpi,        b: Gpi       ) => a === b
      case (a: GmosN,      b: GmosN     ) => a === b
      case (a: GmosS,      b: GmosS     ) => a === b
      case (a: Gnirs,      b: Gnirs     ) => a === b
      case (a: Gsaoi,      b: Gsaoi     ) => a === b
      case (a: Michelle,   b: Michelle  ) => a === b
      case (a: Nici,       b: Nici      ) => a === b
      case (a: Nifs,       b: Nifs      ) => a === b
      case (a: Niri,       b: Niri      ) => a === b
      case (a: Phoenix,    b: Phoenix   ) => a === b
      case (a: Trecs,      b: Trecs     ) => a === b
      case (a: Visitor,    b: Visitor   ) => a === b
      case _                              => false
    }

}
