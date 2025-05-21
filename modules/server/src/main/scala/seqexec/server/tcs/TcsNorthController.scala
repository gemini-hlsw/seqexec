// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Show
import cats.data.NonEmptySet
import cats.implicits._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController
import seqexec.server.tcs.TcsController.AoGuide
import seqexec.server.tcs.TcsController.AoTcsConfig
import seqexec.server.tcs.TcsController.BasicTcsConfig
import seqexec.server.tcs.TcsController.GuiderConfig
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.Subsystem
import seqexec.server.tcs.TcsController.TcsConfig
import shapeless.tag.@@

trait TcsNorthController[F[_]] {
  import TcsNorthController._

  def applyConfig(
    subsystems: NonEmptySet[TcsController.Subsystem],
    gaos:       Option[Altair[F]],
    tc:         TcsNorthConfig
  ): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsNorthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit]

}

object TcsNorthController {

  type TcsNorthConfig   = TcsConfig[GuiderConfig @@ AoGuide, AltairController.AltairConfig]
  type TcsNorthAoConfig = AoTcsConfig[GuiderConfig @@ AoGuide, AltairController.AltairConfig]

  implicit val aoGuideShow: Show[GuiderConfig @@ AoGuide] =
    Show.show(_.asInstanceOf[GuiderConfig].show)

  implicit val tcsNorthConfigShow: Show[TcsNorthConfig] = Show.show {
    case x: BasicTcsConfig   => x.show
    case x: TcsNorthAoConfig => x.show
  }

}
