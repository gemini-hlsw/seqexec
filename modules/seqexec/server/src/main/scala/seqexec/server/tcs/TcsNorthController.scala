// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Eq
import cats.data.NonEmptySet
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.TelescopeGuideConfig
import seqexec.server.InstrumentGuide
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.tcs.TcsController.{AoGuide, GuiderConfig, OIConfig, P1Config, P2Config}
import shapeless.tag.@@

trait TcsNorthController[F[_]] {
  import TcsNorthController._

  def applyConfig(subsystems: NonEmptySet[TcsController.Subsystem],
                  gaos: Option[Altair[F]],
                  tc: TcsNorthConfig): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

object TcsNorthController {
  @Lenses
  final case class GuidersConfig(
                                  pwfs1: GuiderConfig@@P1Config,
                                  pwfs2OrAowfs: Either[GuiderConfig@@P2Config, GuiderConfig@@AoGuide],
                                  oiwfs: GuiderConfig@@OIConfig
                                )

  object GuidersConfig
  {
    implicit val pwfs1Eq: Eq[GuiderConfig@@P1Config] = Eq[GuiderConfig].contramap(identity)
  }

  @Lenses
  final case class TcsNorthConfig(
    gc:  TelescopeGuideConfig,
    tc:  TcsController.TelescopeConfig,
    gds: GuidersConfig,
    agc: TcsController.AGConfig,
    gaos: Option[AltairConfig],
    inst: InstrumentGuide
  )

  object TcsConfig

}
