// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import cats.Eq
import cats.implicits._
import gem.enum._
import monocle.Lens
import monocle.macros.Lenses

/**
 * Instrument configuration that is specified once per [[gem.Observation Observation]] and is thus
 * the same for every [[gem.Step Step]].
 * @group Configurations
 */
sealed trait StaticConfig
object StaticConfig {

  final case class AcqCam()     extends StaticConfig
  final case class Bhros()      extends StaticConfig
  final case class Ghost()      extends StaticConfig
  final case class Gpi()        extends StaticConfig
  final case class Gsaoi()      extends StaticConfig
  final case class Michelle()   extends StaticConfig
  final case class Nici()       extends StaticConfig
  final case class Nifs()       extends StaticConfig
  final case class Niri()       extends StaticConfig
  final case class Phoenix()    extends StaticConfig
  final case class Trecs()      extends StaticConfig
  final case class Visitor()    extends StaticConfig

  final case class Flamingos2(
    mosPreImaging: MosPreImaging
  ) extends StaticConfig

  object Flamingos2 {
    val Default: Flamingos2 =
      Flamingos2(MosPreImaging.IsNotMosPreImaging)
  }

  import GmosConfig._

  final case class GmosN(
    common:    GmosCommonStaticConfig,
    stageMode: GmosNorthStageMode
  ) extends StaticConfig

  object GmosN extends GmosNorthLenses {

    val Default: GmosN =
      GmosN(
        GmosCommonStaticConfig.Default,
        GmosNorthStageMode.FollowXy
      )

    implicit val EqualGmosN: Eq[GmosN] =
      Eq.by(g => (g.common, g.stageMode))

  }

  trait GmosNorthLenses { this: GmosN.type =>

    /** @group Optics */
    val common: Lens[GmosN, GmosCommonStaticConfig] =
      Lens[GmosN, GmosCommonStaticConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val stageMode: Lens[GmosN, GmosNorthStageMode] =
      Lens[GmosN, GmosNorthStageMode](_.stageMode)(a => _.copy(stageMode = a))

    /** @group Optics */
    val customRois: Lens[GmosN, Set[GmosCustomRoiEntry]] =
      common composeLens GmosCommonStaticConfig.customRois

    /** @group Optics */
    val nodAndShuffle: Lens[GmosN, Option[GmosNodAndShuffle]] =
      common composeLens GmosCommonStaticConfig.nodAndShuffle

  }

  final case class GmosS(
    common:    GmosCommonStaticConfig,
    stageMode: GmosSouthStageMode
  ) extends StaticConfig

  object GmosS extends GmosSouthLenses {

    val Default: GmosS =
      GmosS(
        GmosCommonStaticConfig.Default,
        GmosSouthStageMode.FollowXyz
      )

    implicit val EqualGmosS: Eq[GmosS] =
      Eq.by(g => (g.common, g.stageMode))
  }

  trait GmosSouthLenses { this: GmosS.type =>

    /** @group Optics */
    val common: Lens[GmosS, GmosCommonStaticConfig] =
      Lens[GmosS, GmosCommonStaticConfig](_.common)(a => _.copy(common = a))

    /** @group Optics */
    val stageMode: Lens[GmosS, GmosSouthStageMode] =
      Lens[GmosS, GmosSouthStageMode](_.stageMode)(a => _.copy(stageMode = a))

    lazy val customRois: Lens[GmosS, Set[GmosCustomRoiEntry]] =
      common composeLens GmosCommonStaticConfig.customRois

    lazy val nodAndShuffle: Lens[GmosS, Option[GmosNodAndShuffle]] =
      common composeLens GmosCommonStaticConfig.nodAndShuffle
  }

  @Lenses final case class Gnirs(
    wellDepth: GnirsWellDepth
  ) extends StaticConfig

  object Gnirs {
    val Default: Gnirs =
      Gnirs(GnirsWellDepth.Shallow)
  }

  implicit val EqStaticConfig: Eq[StaticConfig] =
    Eq.fromUniversalEquals

}
