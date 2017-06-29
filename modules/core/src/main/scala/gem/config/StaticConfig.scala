// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum.{GmosNorthStageMode, GmosSouthStageMode, Instrument, MosPreImaging}

sealed abstract class StaticConfig {
  type I <: Instrument with Singleton
  def instrument: I
}

object StaticConfig {
  type Aux[I0] = StaticConfig { type I = I0 }
  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends StaticConfig {
    type I = I0
  }
}

final case class PhoenixStaticConfig()    extends StaticConfig.Impl(Instrument.Phoenix)
final case class MichelleStaticConfig()   extends StaticConfig.Impl(Instrument.Michelle)
final case class GnirsStaticConfig()      extends StaticConfig.Impl(Instrument.Gnirs)
final case class NiriStaticConfig()       extends StaticConfig.Impl(Instrument.Niri)
final case class TrecsStaticConfig()      extends StaticConfig.Impl(Instrument.Trecs)
final case class NiciStaticConfig()       extends StaticConfig.Impl(Instrument.Nici)
final case class NifsStaticConfig()       extends StaticConfig.Impl(Instrument.Nifs)
final case class GpiStaticConfig()        extends StaticConfig.Impl(Instrument.Gpi)
final case class GsaoiStaticConfig()      extends StaticConfig.Impl(Instrument.Gsaoi)
final case class AcqCamStaticConfig()     extends StaticConfig.Impl(Instrument.AcqCam)
final case class BhrosStaticConfig()      extends StaticConfig.Impl(Instrument.Bhros)
final case class VisitorStaticConfig()    extends StaticConfig.Impl(Instrument.Visitor)

final case class F2StaticConfig(
  mosPreImaging: MosPreImaging
) extends StaticConfig.Impl(Instrument.Flamingos2)

object F2StaticConfig {
  val Default: F2StaticConfig =
    F2StaticConfig(MosPreImaging.IsNotMosPreImaging)
}

import Gmos._

final case class GmosNorthStaticConfig(
  common:    GmosCommonStaticConfig,
  stageMode: GmosNorthStageMode
) extends StaticConfig.Impl(Instrument.GmosN)

object GmosNorthStaticConfig {
  val Default: GmosNorthStaticConfig =
    GmosNorthStaticConfig(
      GmosCommonStaticConfig.Default,
      GmosNorthStageMode.FollowXy
    )
}

final case class GmosSouthStaticConfig(
  common:    GmosCommonStaticConfig,
  stageMode: GmosSouthStageMode
) extends StaticConfig.Impl(Instrument.GmosS)

object GmosSouthStaticConfig {
  val Default: GmosSouthStaticConfig =
    GmosSouthStaticConfig(
      GmosCommonStaticConfig.Default,
      GmosSouthStageMode.FollowXyz
    )
}
