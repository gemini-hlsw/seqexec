// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.altair.{Altair, AltairController}
import seqexec.server.tcs.TcsController.{AoGuide, AoTcsConfig, GuiderConfig, InstrumentOffset, Subsystem, TcsConfig}
import shapeless.tag.@@

trait TcsNorthController[F[_]] {
  import TcsNorthController._

  def applyConfig(subsystems: NonEmptySet[TcsController.Subsystem],
                  gaos: Option[Altair[F]],
                  tc: TcsNorthConfig): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(subsystems: NonEmptySet[Subsystem], tcsConfig: TcsNorthConfig)
         (stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit]

}

object TcsNorthController {

  type TcsNorthConfig = TcsConfig[GuiderConfig@@AoGuide, AltairController.AltairConfig]
  type TcsNorthAoConfig = AoTcsConfig[GuiderConfig@@AoGuide, AltairController.AltairConfig]

}
