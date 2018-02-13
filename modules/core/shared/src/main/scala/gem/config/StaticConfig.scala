// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum.{GmosNorthStageMode, GmosSouthStageMode, GnirsWellDepth, Instrument, MosPreImaging}
import monocle.Lens
import monocle.macros.Lenses

/**
 * Instrument configuration that is specified once per [[gem.Observation Observation]] and is thus
 * the same for every [[gem.Step Step]].
 * @group Configurations
 */
sealed abstract class StaticConfig {
  type I <: Instrument with Singleton
  val instrument: Instrument.Aux[I]
}

object StaticConfig {

  type Aux[I0] = StaticConfig { type I = I0 }

  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: Instrument.Aux[I0]) extends StaticConfig {
    type I = I0
  }

  final case class AcqCam()     extends StaticConfig.Impl(Instrument.AcqCam)
  final case class Bhros()      extends StaticConfig.Impl(Instrument.Bhros)
  final case class Ghost()      extends StaticConfig.Impl(Instrument.Ghost)
  final case class Gpi()        extends StaticConfig.Impl(Instrument.Gpi)
  final case class Gsaoi()      extends StaticConfig.Impl(Instrument.Gsaoi)
  final case class Michelle()   extends StaticConfig.Impl(Instrument.Michelle)
  final case class Nici()       extends StaticConfig.Impl(Instrument.Nici)
  final case class Nifs()       extends StaticConfig.Impl(Instrument.Nifs)
  final case class Niri()       extends StaticConfig.Impl(Instrument.Niri)
  final case class Phoenix()    extends StaticConfig.Impl(Instrument.Phoenix)
  final case class Trecs()      extends StaticConfig.Impl(Instrument.Trecs)
  final case class Visitor()    extends StaticConfig.Impl(Instrument.Visitor)

  final case class F2(
    mosPreImaging: MosPreImaging
  ) extends StaticConfig.Impl(Instrument.Flamingos2)

  object F2 {
    val Default: F2 =
      F2(MosPreImaging.IsNotMosPreImaging)
  }

  import GmosConfig._

  @Lenses final case class GmosNorth(
    common:    GmosCommonStaticConfig,
    stageMode: GmosNorthStageMode
  ) extends StaticConfig.Impl(Instrument.GmosN)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GmosNorth extends GmosNorthLenses {

    val Default: GmosNorth =
      GmosNorth(
        GmosCommonStaticConfig.Default,
        GmosNorthStageMode.FollowXy
      )

  }

  trait GmosNorthLenses { this: GmosNorth.type =>

    lazy val customRois: Lens[GmosNorth, Set[GmosCustomRoiEntry]] =
      common ^|-> GmosCommonStaticConfig.customRois

    lazy val nodAndShuffle: Lens[GmosNorth, Option[GmosNodAndShuffle]] =
      common ^|-> GmosCommonStaticConfig.nodAndShuffle

  }

  @Lenses final case class GmosSouth(
    common:    GmosCommonStaticConfig,
    stageMode: GmosSouthStageMode
  ) extends StaticConfig.Impl(Instrument.GmosS)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GmosSouth extends GmosSouthLenses {
    val Default: GmosSouth =
      GmosSouth(
        GmosCommonStaticConfig.Default,
        GmosSouthStageMode.FollowXyz
      )
  }

  trait GmosSouthLenses { this: GmosSouth.type =>

    lazy val customRois: Lens[GmosSouth, Set[GmosCustomRoiEntry]] =
      common ^|-> GmosCommonStaticConfig.customRois

    lazy val nodAndShuffle: Lens[GmosSouth, Option[GmosNodAndShuffle]] =
      common ^|-> GmosCommonStaticConfig.nodAndShuffle
  }

  @Lenses final case class Gnirs(
    wellDepth: GnirsWellDepth
  ) extends StaticConfig.Impl(Instrument.Gnirs)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object Gnirs {
    val Default: Gnirs =
      Gnirs(GnirsWellDepth.Shallow)
  }

}
