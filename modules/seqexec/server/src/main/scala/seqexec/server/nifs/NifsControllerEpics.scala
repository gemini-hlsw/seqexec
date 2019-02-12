// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.data.OptionT
import cats.effect.IO
import cats.effect.Timer
import cats.implicits._
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ ReadMode => LegacyReadMode }
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ EngReadMode => LegacyEngReadMode }
import edu.gemini.seqexec.server.nifs.DhsConnected
import org.log4s.getLogger
import scala.concurrent.ExecutionContext
import seqexec.model.dhs.ImageFileId
import seqexec.server.ObserveCommand
import seqexec.server.Progress
import seqexec.server.ProgressUtil
import seqexec.server.SeqexecFailure
import seqexec.server.failUnlessM
import shapeless.tag
import squants.Seconds
import squants.Time
import squants.time.TimeConversions._

object NifsLookupTables {

  val readModeLUT: Map[LegacyReadMode, Int] = Map(
    LegacyReadMode.BRIGHT_OBJECT_SPEC -> 1,
    LegacyReadMode.MEDIUM_OBJECT_SPEC -> 1,
    LegacyReadMode.FAINT_OBJECT_SPEC -> 1
  )

  val engineeringReadModeLUT: Map[LegacyEngReadMode, Int] = Map(
    LegacyEngReadMode.FOWLER_SAMPLING_READOUT -> 1,
    LegacyEngReadMode.LINEAR_READ -> 2
  )
}

object NifsControllerEpics extends NifsController[IO] {

  private val Log = getLogger

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  import NifsController._
  import NifsLookupTables._

  private val epicsSys = NifsEpics.instance

  private def setCoadds(n: Coadds): IO[Unit] =
    epicsSys.dcConfigCmd.setCoadds(n)

  private def setNumberOfResets(r: Option[NumberOfResets]): IO[Unit] =
    r.foldMap(epicsSys.dcConfigCmd.setnumberOfResets)

  private def setNumberOfPeriods(r: Option[NumberOfPeriods]): IO[Unit] =
    r.foldMap(epicsSys.dcConfigCmd.setnumberOfPeriods)

  private def numberOfFowSamples(
    rm: Either[EngReadMode, ReadMode]
  ): Option[NumberOfFowSamples] =
    rm.map {
        case LegacyReadMode.BRIGHT_OBJECT_SPEC => 1
        case LegacyReadMode.MEDIUM_OBJECT_SPEC => 4
        case LegacyReadMode.FAINT_OBJECT_SPEC  => 16
      }
      .map(tag[NumberOfFowSamplesI][Int])
      .toOption

  private def setReadMode(rm: Either[EngReadMode, ReadMode]): IO[Unit] =
    rm.pure[IO].flatMap {
      case Left(e) =>
        engineeringReadModeLUT
          .get(e)
          .map(epicsSys.dcConfigCmd.setReadMode)
          .getOrElse(IO.unit)
      case Right(r) =>
        readModeLUT
          .get(r)
          .map(epicsSys.dcConfigCmd.setReadMode)
          .getOrElse(IO.unit)
    }

  private def setNumberOfSamples(
    samples:    Option[NumberOfSamples],
    fowSamples: Option[NumberOfFowSamples]
  ): IO[Unit] =
    for {
      s <- samples.pure[IO]
      r <- s.orElse(fowSamples).pure[IO]
      v <- r.widen[Int].pure[IO]
      _ <- v.map(epicsSys.dcConfigCmd.setFowlerSamples).getOrElse(IO.unit)
    } yield ()

  private def setPeriod(period: Option[Period]): IO[Unit] =
    (for {
      p <- OptionT(period.pure[IO])
      _ <- OptionT.liftF(epicsSys.dcConfigCmd.setPeriod(p.toDouble))
      _ <- OptionT.liftF(epicsSys.dcConfigCmd.setTimeMode(0))
    } yield ()).value.void

  private def setExposureTime(expTime: ExposureTime): IO[Unit] =
    epicsSys.dcConfigCmd.setExposureTime(expTime.toSeconds) *>
      epicsSys.dcConfigCmd.setPeriod(1) *>
      epicsSys.dcConfigCmd.setTimeMode(1)

  private def configDC(cfg: DCConfig): IO[Unit] =
    setReadMode(cfg.readMode) *>
      setNumberOfSamples(cfg.numberOfSamples, numberOfFowSamples(cfg.readMode)) *>
      setPeriod(cfg.period) *>
      setExposureTime(cfg.exposureTime).whenA(cfg.period.isEmpty) *>
      setCoadds(cfg.coadds) *>
      setNumberOfResets(cfg.numberOfResets) *>
      setNumberOfPeriods(cfg.numberOfPeriods)

  override def applyConfig(config: NifsController.NifsConfig): IO[Unit] =
    configDC(config.dc) *>
      // TODO configure CC
      IO.raiseError(SeqexecFailure.Execution("NFS CC Not implemented"))

  override def observe(fileId: ImageFileId,
                       cfg:    DCConfig): IO[ObserveCommand.Result] = {
    val checkDhs =
      failUnlessM(epicsSys.dhsConnectedAttr.map(_.value() === DhsConnected.Yes),
                  SeqexecFailure.Execution("NIFS is not connected to DHS"))

    IO(Log.info("Start NIFS observe")) *>
      checkDhs *>
      epicsSys.observeCmd.setLabel(fileId) *>
      epicsSys.observeCmd.setTimeout[IO](calcObserveTimeout(cfg)) *>
      epicsSys.observeCmd.post[IO]
  }

  override def endObserve: IO[Unit] =
    IO(Log.info("Send endObserve to NIFS")) *>
      epicsSys.endObserveCmd.setTimeout[IO](DefaultTimeout) *>
      epicsSys.endObserveCmd.mark[IO] *>
      epicsSys.endObserveCmd.post[IO].void

  override def stopObserve: IO[Unit] =
    IO(Log.info("Stop NIFS exposure")) *>
      epicsSys.stopCmd.setTimeout[IO](DefaultTimeout) *>
      epicsSys.stopCmd.mark[IO] *>
      epicsSys.stopCmd.post[IO].void

  override def abortObserve: IO[Unit] =
    IO(Log.info("Abort NIFS exposure")) *>
      epicsSys.abortCmd.setTimeout[IO](DefaultTimeout) *>
      epicsSys.abortCmd.mark[IO] *>
      epicsSys.abortCmd.post[IO].void

  override def observeProgress(total: Time): fs2.Stream[IO, Progress] =
    ProgressUtil.countdown[IO](total, 0.seconds)

  override def calcTotalExposureTime(cfg: DCConfig): IO[Time] =
    epicsSys.exposureTime.map { exp =>
      val MinIntTime = exp.map(Seconds(_)).getOrElse(0.seconds)

      (cfg.exposureTime + MinIntTime) * cfg.coadds.toDouble
    }

  def calcObserveTimeout(cfg: DCConfig): Time = {
    val CoaddOverhead = 2.2
    val TotalOverhead = 300.seconds

    cfg.exposureTime * cfg.coadds.toDouble * CoaddOverhead + TotalOverhead
  }

  // private val ConfigTimeout: Time = Seconds(180)
  private val DefaultTimeout: Time = Seconds(60)

}
