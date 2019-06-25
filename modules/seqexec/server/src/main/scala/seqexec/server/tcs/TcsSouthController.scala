// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data.NonEmptySet
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.TelescopeGuideConfig
import seqexec.server.InstrumentGuide
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController.{AGConfig, GuiderConfig, OIConfig, P1Config, P2Config, Subsystem, TelescopeConfig}
import shapeless.tag.@@

trait TcsSouthController[F[_]] {
  import TcsSouthController._

  def applyConfig(subsystems: NonEmptySet[Subsystem],
                  gaos: Option[Gems[F]],
                  tc: TcsSouthConfig): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

// scalastyle:off
object TcsSouthController {

  @Lenses
  final case class GuidersConfig(
                                  pwfs1: GuiderConfig@@P1Config,
                                  pwfs2: GuiderConfig@@P2Config,
                                  oiwfs: GuiderConfig@@OIConfig
                                )

  object GuidersConfig {
    implicit val pwfs1Eq: Eq[GuiderConfig@@P1Config] = Eq[GuiderConfig].contramap(identity)
  }

  @Lenses
  final case class TcsSouthConfig(
                              gc:  TelescopeGuideConfig,
                              tc:  TelescopeConfig,
                              gds: GuidersConfig,
                              agc: AGConfig,
                              gaos: Option[GemsConfig],
                              inst: InstrumentGuide
                            )

  object TcsSouthConfig

}
// scalastyle:on
