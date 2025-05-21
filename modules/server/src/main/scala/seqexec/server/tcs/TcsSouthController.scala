// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Show
import cats.data.NonEmptySet
import cats.implicits._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController.AoTcsConfig
import seqexec.server.tcs.TcsController.BasicTcsConfig
import seqexec.server.tcs.TcsController.GuiderConfig
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.Subsystem
import seqexec.server.tcs.TcsController.TcsConfig
import shapeless.tag.@@

trait TcsSouthController[F[_]] {
  import TcsSouthController._

  def applyConfig(
    subsystems: NonEmptySet[Subsystem],
    gaos:       Option[Gems[F]],
    tc:         TcsSouthConfig
  ): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsSouthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit]

}

object TcsSouthController {

  trait CWFS1Config
  trait CWFS2Config
  trait CWFS3Config
  trait ODGW1Config
  trait ODGW2Config
  trait ODGW3Config
  trait ODGW4Config

  final case class GemsGuiders(
    cwfs1: GuiderConfig @@ CWFS1Config,
    cwfs2: GuiderConfig @@ CWFS2Config,
    cwfs3: GuiderConfig @@ CWFS3Config,
    odgw1: GuiderConfig @@ ODGW1Config,
    odgw2: GuiderConfig @@ ODGW2Config,
    odgw3: GuiderConfig @@ ODGW3Config,
    odgw4: GuiderConfig @@ ODGW4Config
  )

  type TcsSouthConfig   = TcsConfig[GemsGuiders, GemsConfig]
  type TcsSouthAoConfig = AoTcsConfig[GemsGuiders, GemsConfig]

  implicit val aoGuideShow: Show[GemsGuiders] = Show.show { x =>
    s"(cwfs1 = ${(x.cwfs1: GuiderConfig).show}, cwfs2 = ${(x.cwfs2: GuiderConfig).show}, cwfs3 = ${(x.cwfs3: GuiderConfig).show}, odgw1 = ${(x.odgw1: GuiderConfig).show}, odgw2 = ${(x.odgw2: GuiderConfig).show}, odgw3 = ${(x.odgw3: GuiderConfig).show}, odgw4 = ${(x.odgw4: GuiderConfig).show})"
  }

  implicit val tcsSouthConfigShow: Show[TcsSouthConfig] = Show.show {
    case x: BasicTcsConfig   => x.show
    case x: TcsSouthAoConfig => x.show
  }

}
