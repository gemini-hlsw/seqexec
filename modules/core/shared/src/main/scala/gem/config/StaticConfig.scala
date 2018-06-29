// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import cats.Eq
import gem.enum.{GmosNorthStageMode, GmosSouthStageMode, GnirsWellDepth, MosPreImaging}
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

  @Lenses final case class GmosN(
    common:    GmosCommonStaticConfig,
    stageMode: GmosNorthStageMode
  ) extends StaticConfig

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GmosN extends GmosNorthLenses {

    val Default: GmosN =
      GmosN(
        GmosCommonStaticConfig.Default,
        GmosNorthStageMode.FollowXy
      )

  }

  trait GmosNorthLenses { this: GmosN.type =>

    lazy val customRois: Lens[GmosN, Set[GmosCustomRoiEntry]] =
      common ^|-> GmosCommonStaticConfig.customRois

    lazy val nodAndShuffle: Lens[GmosN, Option[GmosNodAndShuffle]] =
      common ^|-> GmosCommonStaticConfig.nodAndShuffle

  }

  @Lenses final case class GmosS(
    common:    GmosCommonStaticConfig,
    stageMode: GmosSouthStageMode
  ) extends StaticConfig

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GmosS extends GmosSouthLenses {
    val Default: GmosS =
      GmosS(
        GmosCommonStaticConfig.Default,
        GmosSouthStageMode.FollowXyz
      )
  }

  trait GmosSouthLenses { this: GmosS.type =>

    lazy val customRois: Lens[GmosS, Set[GmosCustomRoiEntry]] =
      common ^|-> GmosCommonStaticConfig.customRois

    lazy val nodAndShuffle: Lens[GmosS, Option[GmosNodAndShuffle]] =
      common ^|-> GmosCommonStaticConfig.nodAndShuffle
  }

  @Lenses final case class Gnirs(
    wellDepth: GnirsWellDepth
  ) extends StaticConfig

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object Gnirs {
    val Default: Gnirs =
      Gnirs(GnirsWellDepth.Shallow)
  }

  implicit val EqStaticConfig: Eq[StaticConfig] =
    Eq.fromUniversalEquals

}
