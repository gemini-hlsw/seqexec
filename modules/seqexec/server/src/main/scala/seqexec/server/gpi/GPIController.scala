// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.{Eq, Show}
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import scala.concurrent.duration.Duration
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqAction
import giapi.client.commands.CommandResult
import giapi.client.gpi.GPIClient
import org.log4s.getLogger
import edu.gemini.aspen.giapi.commands.{DefaultConfiguration}
import mouse.boolean._

final case class GPIController(gpiClient: GPIClient[IO]) {
  import GPIController._
  private val Log = getLogger

  def gpiConfig(config: GPIConfig): SeqAction[CommandResult] = {
    val giapiApply = DefaultConfiguration
      .configurationBuilder()
      .withConfiguration(
        "gpi:selectAdc.deploy",
        (config.adc === edu.gemini.spModel.gemini.gpi.Gpi.Adc.IN)
          .fold(1, 0)
          .show)
    EitherT.liftF(gpiClient.genericApply(giapiApply.build()))
  }

  def applyConfig(config: GPIConfig): SeqAction[Unit] =
    for {
      _ <- EitherT.liftF(IO.apply(Log.debug("Start GPI configuration")))
      _ <- EitherT.liftF(IO.apply(Log.debug(s"GPI configuration $config")))
      _ <- gpiConfig(config)
      _ <- EitherT.liftF(IO(Log.debug("Completed GPI configuration")))
    } yield ()

  def observe(fileId: ImageFileId): SeqAction[ImageFileId] =
    EitherT(gpiClient.observe(fileId).map(_ => fileId.asRight))

  def endObserve: SeqAction[Unit] =
    SeqAction.void
}

object GPIController {

  type Adc = edu.gemini.spModel.gemini.gpi.Gpi.Adc

  implicit val adcEq: Eq[Adc] = Eq.by(_.displayValue)

  type ObservingMode = edu.gemini.spModel.gemini.gpi.Gpi.ObservingMode

  implicit val omEq: Eq[ObservingMode] = Eq.by(_.displayValue)

  type Disperser = edu.gemini.spModel.gemini.gpi.Gpi.Disperser

  implicit val dispEq: Eq[Disperser] = Eq.by(_.displayValue)

  type Shutter = edu.gemini.spModel.gemini.gpi.Gpi.Shutter

  implicit val shEq: Eq[Shutter] = Eq.by(_.displayValue)

  type ArtificialSource = edu.gemini.spModel.gemini.gpi.Gpi.ArtificialSource

  implicit val asEq: Eq[ArtificialSource] = Eq.by(_.displayValue)

  type PupilCamera = edu.gemini.spModel.gemini.gpi.Gpi.PupilCamera

  implicit val pcEq: Eq[PupilCamera] = Eq.by(_.displayValue)

  final case class AOFlags(useAo: Boolean,
                           useCal: Boolean,
                           aoOptimize: Boolean,
                           alignFpm: Boolean)

  object AOFlags {
    implicit val eq: Eq[AOFlags]     = Eq.fromUniversalEquals
    implicit val show: Show[AOFlags] = Show.fromToString
  }

  final case class ArtificialSources(ir: ArtificialSource,
                                     vis: ArtificialSource,
                                     sc: ArtificialSource,
                                     attenuation: Double)

  object ArtificialSources {
    implicit val eq: Eq[ArtificialSources] =
      Eq.by(x => (x.ir, x.vis, x.sc, x.attenuation))
    implicit val show: Show[ArtificialSources] = Show.fromToString
  }

  final case class Shutters(entranceShutter: Shutter,
                            calEntranceShutter: Shutter,
                            scienceArmShutter: Shutter,
                            referenceArmShutter: Shutter)

  object Shutters {
    implicit val eq: Eq[Shutters] = Eq.by(
      x =>
        (x.entranceShutter,
         x.calEntranceShutter,
         x.scienceArmShutter,
         x.referenceArmShutter))
    implicit val show: Show[Shutters] = Show.fromToString
  }

  final case class GPIConfig(adc: Adc,
                             expTime: Duration,
                             coAdds: Int,
                             mode: ObservingMode,
                             disperser: Disperser,
                             disperserAngle: Double,
                             shutters: Shutters,
                             asu: ArtificialSources,
                             pc: PupilCamera,
                             aoFlags: AOFlags)

  object GPIConfig {
    private implicit val durationEq: Eq[Duration] = Eq.by(_.toMillis)
    implicit val eq: Eq[GPIConfig] = Eq.by(
      x =>
        (x.adc,
         x.expTime,
         x.coAdds,
         x.mode,
         x.disperser,
         x.disperserAngle,
         x.shutters,
         x.asu,
         x.pc,
         x.aoFlags))
    implicit val show: Show[GPIConfig] = Show.fromToString
  }
}
