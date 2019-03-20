// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import fs2.Stream
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.KeywordsClient
import edu.gemini.spModel.config2.Config
import gem.enum.LightSinkName
import seqexec.model.enum.Instrument
import squants.{Length, Time}

trait InstrumentSystem[F[_]] extends System[F] with InstrumentGuide {
  override val resource: Instrument
  // The name used for this instrument in the science fold configuration
  def sfName(config: Config): LightSinkName
  val contributorName: String
  val observeControl: InstrumentSystem.ObserveControl

  def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result]
  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: Config): F[Time]
  def keywordsClient: KeywordsClient[F]
  def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress]
  override val oiOffsetGuideThreshold: Option[Length] = None
  override def instrument: Instrument = resource
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

  final case class ElapsedTime(self: Time) extends AnyVal
}
