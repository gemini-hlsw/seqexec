// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.dhs.ImageFileId
import edu.gemini.spModel.config2.Config
import squants.Time

trait InstrumentSystem extends System {
  // The name used for this instrument in the science fold configuration
  val sfName: String
  val contributorName: String
  val dhsInstrumentName: String
  val observeControl: InstrumentSystem.ObserveControl
  def observe(config: Config): SeqObserve[ImageFileId, ObserveCommand.Result]
  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: Config): Time

  override def notifyObserveStart = SeqAction.void
}

object InstrumentSystem {
  sealed trait ObserveControl
  object Uncontrollable extends ObserveControl
  final case class StopObserveCmd(self: SeqAction[Unit]) extends AnyVal
  final case class AbortObserveCmd(self: SeqAction[Unit]) extends AnyVal
  final case class PauseObserveCmd(self: SeqAction[Unit]) extends AnyVal
  final case class ContinuePausedCmd(self: Time => SeqAction[ObserveCommand.Result]) extends AnyVal
  final case class StopPausedCmd(self: SeqAction[ObserveCommand.Result]) extends AnyVal
  final case class AbortPausedCmd(self: SeqAction[ObserveCommand.Result]) extends AnyVal
  final case class OpticControl(stop: StopObserveCmd,
                                abort: AbortObserveCmd,
                                pause: PauseObserveCmd,
                                continue: ContinuePausedCmd,
                                stopPaused: StopPausedCmd,
                                abortPaused: AbortPausedCmd) extends ObserveControl
  // Special class for infrared instrument, because they cannot pause/resume
  final case class InfraredControl(stop: StopObserveCmd,
                                   abort: AbortObserveCmd) extends ObserveControl
}
