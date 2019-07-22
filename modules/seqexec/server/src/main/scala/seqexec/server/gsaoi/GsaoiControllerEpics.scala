// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.data.OptionT
import cats.effect.{IO, Timer}
import cats.implicits._
import mouse.boolean._
import edu.gemini.seqexec.server.gsaoi.DhsConnected
import edu.gemini.spModel.gemini.gsaoi.Gsaoi.{Filter, ReadMode, Roi, UtilityWheel}
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.EpicsCodex._
import seqexec.server.gsaoi.GsaoiController.{DCConfig, GsaoiConfig}
import seqexec.server.{EpicsUtil, Progress, SeqexecFailure, failUnlessM}
import seqexec.server.EpicsUtil.applyParam
import squants.{Seconds, Time}
import squants.time.TimeConversions._

import scala.concurrent.ExecutionContext

object GsaoiControllerEpics {

  private val ConfigTimeout: Time  = Seconds(400)
  private val DefaultTimeout: Time = Seconds(60)

  private val Log = getLogger

  implicit val filterEncoder: EncodeEpicsValue[Filter, String] = EncodeEpicsValue {
    case Filter.BLOCKED      => "Blocked"
    case Filter.BR_GAMMA     => "HI-Brgamm"
    case Filter.CH4_LONG     => "CH4(long)"
    case Filter.CH4_SHORT    => "CH4(short)"
    case Filter.CO           => "CO"
    case Filter.DIFFUSER1    => "Diffuser1"
    case Filter.DIFFUSER2    => "Diffuser2"
    case Filter.FE_II        => "FeII"
    case Filter.H            => "H"
    case Filter.H2_1_0_S_1   => "H2-1-0"
    case Filter.H2_2_1_S_1   => "H2-2-1"
    case Filter.H20_ICE      => "H2O"
    case Filter.H_CONTINUUM  => "H-continuum"
    case Filter.HEI          => "HeI-1.083"
    case Filter.HEI_2P2S     => "HeI-2p2s"
    case Filter.J            => "J"
    case Filter.J_CONTINUUM  => "J-continuum"
    case Filter.K            => "K"
    case Filter.K_CONTINUUM1 => "Ks-continuum"
    case Filter.K_CONTINUUM2 => "Kl-continuum"
    case Filter.K_PRIME      => "K'"
    case Filter.K_SHORT      => "Ks"
    case Filter.PA_BETA      => "HI-Pbeta"
    case Filter.PA_GAMMA     => "HI-Pgamm"
    case Filter.Z            => "Z"
  }

  implicit val windowCoverEncoder: EncodeEpicsValue[WindowCover, String] = EncodeEpicsValue {
    case WindowCover.Opened => "Opened"
    case WindowCover.Closed => "Closed"
  }

  implicit val utilityWheelEncoder: EncodeEpicsValue[UtilityWheel, String] = EncodeEpicsValue {
    case UtilityWheel.CLEAR             => "Clear"
    case UtilityWheel.EXTRAFOCAL_LENS_1 => "FocusExtender"
    case UtilityWheel.EXTRAFOCAL_LENS_2 => "FocusRetractor"
    case UtilityWheel.PUPIL_IMAGER      => "PupilViewer"
  }

  implicit val exposureTimeEncoder: EncodeEpicsValue[Time, Double] = EncodeEpicsValue(_.toSeconds)

  implicit val roiEncoder: EncodeEpicsValue[Roi, String] = EncodeEpicsValue(_.sequenceValue)

  private def fowlerSamplesFromMode(rm: ReadMode): Int = rm match {
    case ReadMode.BRIGHT     => 1
    case ReadMode.FAINT      => 4
    case ReadMode.VERY_FAINT => 8
  }

  // This looks a bit silly, but I prefer to keep it in case the definition is changed in the future.
  private def readModeFromMode(rm: ReadMode): String = rm match {
    case ReadMode.BRIGHT     => "FOWLER"
    case ReadMode.FAINT      => "FOWLER"
    case ReadMode.VERY_FAINT => "FOWLER"
  }

  final case class EpicsGsaoiConfig(
    filter: String,
    utilityWheel: String,
    windowCover: String,
    readMode: String,
    roi: String,
    coadds: Int,
    exposureTime: Double,
    fowlerSamples: Int,
    guiding: Boolean
  )

  def apply(): GsaoiController[IO] = new GsaoiController[IO] {

    private val epicsSys = GsaoiEpics.instance

    override def applyConfig(config: GsaoiConfig): IO[Unit] = retrieveConfig.flatMap { current =>
      val ccParams = List(
        applyParam(current.filter, encode(config.cc.filter), epicsSys.ccConfigCmd.setFilter),
        applyParam(current.utilityWheel, encode(config.cc.utilityWheel), epicsSys.ccConfigCmd.setUtilWheel),
        applyParam(current.windowCover, encode(config.cc.windowCover), epicsSys.ccConfigCmd.setWindowCover)
      ).flattenOption

      val dcParams = List(
        applyParam(current.coadds, config.dc.coadds, epicsSys.dcConfigCmd.setNumberOfCoadds),
        applyParam(current.exposureTime, encode(config.dc.exposureTime), epicsSys.dcConfigCmd.setExposureTime),
        applyParam(current.fowlerSamples, fowlerSamplesFromMode(config.dc.readMode),
          epicsSys.dcConfigCmd.setFowlerSamples),
        applyParam(current.readMode, readModeFromMode(config.dc.readMode), epicsSys.dcConfigCmd.setReadMode),
        applyParam(current.roi, encode(config.dc.roi), epicsSys.dcConfigCmd.setRoi)
      ).flattenOption

      val guideOff: IO[Unit] = (
        epicsSys.endGuideCmd.mark[IO] *>
          epicsSys.endGuideCmd.setTimeout[IO](DefaultTimeout) *>
          epicsSys.endGuideCmd.post[IO].void *>
          epicsSys.waitForGuideOff
      ).whenA(current.guiding)

      val guideOn: IO[Unit] = (
        epicsSys.guideCmd.mark[IO] *>
          epicsSys.guideCmd.setTimeout[IO](DefaultTimeout) *>
          epicsSys.guideCmd.post[IO] *>
          epicsSys.waitForGuideOn
      ).whenA(current.guiding)

      IO(Log.info("Start Gsaoi configuration")) *>
        IO(Log.debug(s"Gsaoi configuration: ${config.show}")) *>
        guideOff.whenA(ccParams.nonEmpty || dcParams.nonEmpty) *>
        ( ccParams.sequence *>
          epicsSys.ccConfigCmd.setTimeout[IO](ConfigTimeout) *>
          epicsSys.ccConfigCmd.post[IO].void
        ).unlessA(ccParams.isEmpty) *>
        ( dcParams.sequence *>
          epicsSys.dcConfigCmd.setTimeout[IO](ConfigTimeout) *>
          epicsSys.dcConfigCmd.post[IO].void
        ).unlessA(dcParams.isEmpty) *>
        guideOn.whenA(ccParams.nonEmpty || dcParams.nonEmpty) *>
        IO(Log.info("Completed Gsaoi configuration"))
    }

    override def observe(fileId: ImageFileId, cfg: GsaoiController.DCConfig): IO[ObserveCommandResult] = {
      val checkDhs: IO[Unit] = failUnlessM[IO](
        epicsSys.dhsConnected.map(_.exists(_ === DhsConnected.Yes)),
        SeqexecFailure.Execution("GSAOI is not connected to DHS")
      )

      IO(Log.info("Start GSAOI observe")) *>
        checkDhs *>
        epicsSys.observeCmd.setLabel(fileId) *>
        epicsSys.observeCmd.setTimeout[IO](calcObserveTimeout(cfg)) *>
        epicsSys.observeCmd.post[IO]
    }

    // GSAOI endObserve is a NOP with no CAR associated
    override def endObserve: IO[Unit] =
      IO(Log.info("endObserve for GSAOI skipped"))

    override def stopObserve: IO[Unit] =
      IO(Log.info("Stop GSAOI exposure")) *>
        epicsSys.stopCmd.setTimeout[IO](DefaultTimeout) *>
        epicsSys.stopCmd.mark[IO] *>
        epicsSys.stopCmd.post[IO].void

    override def abortObserve: IO[Unit] =
      IO(Log.info("Stop GSAOI exposure")) *>
        epicsSys.abortCmd.setTimeout[IO](DefaultTimeout) *>
        epicsSys.abortCmd.mark[IO] *>
        epicsSys.abortCmd.post[IO].void

    override def observeProgress(total: Time): fs2.Stream[IO, Progress] = {
      implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)
      val rem = (
        for {
          remTime    <- OptionT(epicsSys.countdown).map(_.seconds)
          coaddsDone <- OptionT(epicsSys.coaddsDone)
          coadds     <- OptionT(epicsSys.coadds)
          expTime    <- OptionT(epicsSys.requestedExposureTime).map(_.seconds)
        } yield (coaddsDone<coadds).fold(coadds-coaddsDone-1, 0)*expTime + remTime
      ).value

      EpicsUtil.countdown[IO](total, rem, epicsSys.observeState)
    }

    def calcObserveTimeout(cfg: DCConfig): Time = {
      val factor = 2.2
      val overhead = 300.seconds

      cfg.exposureTime * cfg.coadds.toDouble * factor + overhead
    }

    private def getStatusVal[A](get: IO[Option[A]], name: String): IO[A] =
      get.flatMap(_.map(IO(_)).getOrElse(IO.raiseError(SeqexecFailure.Unexpected(s"Unable to read $name from TCS."))))

    private def retrieveConfig: IO[EpicsGsaoiConfig] = for{
      fl <- getStatusVal(epicsSys.filter, "filter")
      uw <- getStatusVal(epicsSys.utilWheel, "utility wheel")
      wc <- getStatusVal(epicsSys.windowCover, "window cover")
      rm <- getStatusVal(epicsSys.readMode, "read mode")
      ro <- getStatusVal(epicsSys.roi, "ROI")
      co <- getStatusVal(epicsSys.coadds, "coadds")
      et <- getStatusVal(epicsSys.requestedExposureTime, "exposure time")
      fo <- getStatusVal(epicsSys.numberOfFowlerSamples, "number of fowler samples")
      gd <- getStatusVal(epicsSys.guiding, "guide state")
    } yield EpicsGsaoiConfig(fl, uw, wc, rm, ro, co, et, fo, gd)

  }
}
