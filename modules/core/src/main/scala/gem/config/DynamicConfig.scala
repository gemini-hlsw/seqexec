// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum._
import java.time.Duration
import scalaz._

/**
 * Instrument configuration that is specified for each [[gem.Step Step]].
 * @group Configurations
 */
sealed abstract class DynamicConfig extends Product with Serializable {

  type I <: Instrument with Singleton
  def instrument: I

  def smartGcalKey: Option[DynamicConfig.SmartGcalKey] =
    this match {
      case f2: DynamicConfig.F2 => Some(f2.key)
      case _                   => None
    }

}
object DynamicConfig {

  type Aux[I0] = DynamicConfig { type I = I0 }

  sealed trait SmartGcalKey
  object SmartGcalKey {

    final case class F2(
      disperser: F2Disperser,
      filter:    F2Filter,
      fpu:       F2FpUnit
    ) extends SmartGcalKey

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


  /** @group Constructors */
  final case class F2(
    disperser:     F2Disperser,
    exposureTime:  Duration,
    filter:        F2Filter,
    fpu:           F2FpUnit,
    lyotWheel:     F2LyotWheel,
    readMode:      F2ReadMode,
    windowCover:   F2WindowCover
  ) extends DynamicConfig.Impl(Instrument.Flamingos2) {

    def key: SmartGcalKey.F2 =
      SmartGcalKey.F2(disperser, filter, fpu)
  }

  import Gmos._

  /** @group Constructors */
  final case class GmosNorth(
    common:  GmosCommonDynamicConfig,
    grating: Option[GmosGrating[GmosNorthDisperser]],
    filter:  Option[GmosNorthFilter],
    fpu:     Option[GmosCustomMask \/ GmosNorthFpu]
  ) extends DynamicConfig.Impl(Instrument.GmosN)

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
  ) extends DynamicConfig.Impl(Instrument.GmosS)

  object GmosSouth {
    val Default: GmosSouth =
      GmosSouth(GmosCommonDynamicConfig.Default, None, None, None)
  }

}
