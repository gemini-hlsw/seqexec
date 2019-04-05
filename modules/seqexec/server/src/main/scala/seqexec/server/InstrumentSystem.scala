// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  val observeControl: InstrumentSystem.ObserveControl[F]

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

  final case class StopObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class AbortObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class PauseObserveCmd[F[_]](self: SeqActionF[F, Unit])
  final case class ContinuePausedCmd[F[_]](
      self: Time => SeqActionF[F, ObserveCommand.Result])
  final case class StopPausedCmd[F[_]](self: SeqActionF[F, ObserveCommand.Result])
  final case class AbortPausedCmd[F[_]](self: SeqActionF[F, ObserveCommand.Result])

  sealed trait ObserveControl[F[_]] extends Product with Serializable
  final case class Uncontrollable[F[_]]() extends ObserveControl[F]
  final case class OpticControl[F[_]](stop: StopObserveCmd[F],
                                abort: AbortObserveCmd[F],
                                pause: PauseObserveCmd[F],
                                continue: ContinuePausedCmd[F],
                                stopPaused: StopPausedCmd[F],
                                abortPaused: AbortPausedCmd[F])
      extends ObserveControl[F]
  // Special class for infrared instrument, because they cannot pause/resume
  final case class InfraredControl[F[_]](stop: StopObserveCmd[F], abort: AbortObserveCmd[F])
      extends ObserveControl[F]

  final case class ElapsedTime(self: Time) extends AnyVal
}
