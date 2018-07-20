// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.KeywordsClient
import edu.gemini.spModel.config2.Config
import squants.Time

trait InstrumentSystem[F[_]] extends System[F] {
  // The name used for this instrument in the science fold configuration
  val sfName: String
  val contributorName: String
  val observeControl: InstrumentSystem.ObserveControl

  def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result]
  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: Config): Time
  def keywordsClient: KeywordsClient[IO]
}

object InstrumentSystem {

  final case class StopObserveCmd(self: SeqAction[Unit])
  final case class AbortObserveCmd(self: SeqAction[Unit])
  final case class PauseObserveCmd(self: SeqAction[Unit])
  final case class ContinuePausedCmd(
      self: Time => SeqAction[ObserveCommand.Result])
  final case class StopPausedCmd(self: SeqAction[ObserveCommand.Result])
  final case class AbortPausedCmd(self: SeqAction[ObserveCommand.Result])

  sealed trait ObserveControl
  case object Uncontrollable extends ObserveControl
  final case class OpticControl(stop: StopObserveCmd,
                                abort: AbortObserveCmd,
                                pause: PauseObserveCmd,
                                continue: ContinuePausedCmd,
                                stopPaused: StopPausedCmd,
                                abortPaused: AbortPausedCmd)
      extends ObserveControl
  // Special class for infrared instrument, because they cannot pause/resume
  final case class InfraredControl(stop: StopObserveCmd, abort: AbortObserveCmd)
      extends ObserveControl
}
