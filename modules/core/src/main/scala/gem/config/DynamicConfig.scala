// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum._

import java.time.Duration

import scalaz._

sealed trait SmartGcalKey

sealed abstract class DynamicConfig extends Product with Serializable {

  type I <: Instrument with Singleton
  def instrument: I

  def smartGcalKey: Option[SmartGcalKey] =
    this match {
      case f2: F2DynamicConfig => Some(f2.key)
      case _                   => None
    }

}
object DynamicConfig {
  type Aux[I0] = DynamicConfig { type I = I0 }

  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends DynamicConfig {
    type I = I0
  }
}

final case class AcqCamDynamicConfig()   extends DynamicConfig.Impl(Instrument.AcqCam)
final case class BhrosDynamicConfig()    extends DynamicConfig.Impl(Instrument.Bhros)
final case class GnirsDynamicConfig()    extends DynamicConfig.Impl(Instrument.Gnirs)
final case class GpiDynamicConfig()      extends DynamicConfig.Impl(Instrument.Gpi)
final case class GsaoiDynamicConfig()    extends DynamicConfig.Impl(Instrument.Gsaoi)
final case class MichelleDynamicConfig() extends DynamicConfig.Impl(Instrument.Michelle)
final case class NiciDynamicConfig()     extends DynamicConfig.Impl(Instrument.Nici)
final case class NifsDynamicConfig()     extends DynamicConfig.Impl(Instrument.Nifs)
final case class NiriDynamicConfig()     extends DynamicConfig.Impl(Instrument.Niri)
final case class PhoenixDynamicConfig()  extends DynamicConfig.Impl(Instrument.Phoenix)
final case class TrecsDynamicConfig()    extends DynamicConfig.Impl(Instrument.Trecs)
final case class VisitorDynamicConfig()  extends DynamicConfig.Impl(Instrument.Visitor)

final case class F2SmartGcalKey(
  disperser: F2Disperser,
  filter:    F2Filter,
  fpu:       F2FpUnit
) extends SmartGcalKey

final case class F2DynamicConfig(
  disperser:     F2Disperser,
  exposureTime:  Duration,
  filter:        F2Filter,
  fpu:           F2FpUnit,
  lyotWheel:     F2LyotWheel,
  readMode:      F2ReadMode,
  windowCover:   F2WindowCover
) extends DynamicConfig {

  type I = Instrument.Flamingos2.type
  def instrument: I = valueOf[I]

  def key: F2SmartGcalKey =
    F2SmartGcalKey(disperser, filter, fpu)
}

import Gmos._

final case class GmosNorthDynamicConfig(
  common:  GmosCommonDynamicConfig,
  grating: Option[GmosGrating[GmosNorthDisperser]],
  filter:  Option[GmosNorthFilter],
  fpu:     Option[GmosCustomMask \/ GmosNorthFpu],
) extends DynamicConfig {

  type I = Instrument.GmosN.type
  def instrument: I = valueOf[I]
}

object GmosNorthDynamicConfig {
  val Default: GmosNorthDynamicConfig =
    GmosNorthDynamicConfig(GmosCommonDynamicConfig.Default, None, None, None)
}


final case class GmosSouthDynamicConfig(
  common:  GmosCommonDynamicConfig,
  grating: Option[GmosGrating[GmosSouthDisperser]],
  filter:  Option[GmosSouthFilter],
  fpu:     Option[GmosCustomMask \/ GmosSouthFpu],
) extends DynamicConfig {

  type I = Instrument.GmosS.type
  def instrument: I = valueOf[I]
}

object GmosSouthDynamicConfig {
  val Default: GmosSouthDynamicConfig =
    GmosSouthDynamicConfig(GmosCommonDynamicConfig.Default, None, None, None)
}

