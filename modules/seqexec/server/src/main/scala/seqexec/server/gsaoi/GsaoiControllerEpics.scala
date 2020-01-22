// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.effect.Timer
import cats.effect.Async
import cats.implicits._
import edu.gemini.epics.acm.CarStateGeneric
import mouse.boolean._
import edu.gemini.seqexec.server.gsaoi.DhsConnected
import edu.gemini.spModel.gemini.gsaoi.Gsaoi.{Filter, ReadMode, Roi, UtilityWheel}
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ObserveStage
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.EpicsCodex._
import seqexec.server.gsaoi.GsaoiController.{DCConfig, GsaoiConfig}
import seqexec.server.{EpicsUtil, Progress, SeqexecFailure, failUnlessM}
import seqexec.server.EpicsUtil.applyParam
import squants.Time
import squants.time.TimeConversions._

import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}

import scala.concurrent.duration.FiniteDuration

object GsaoiControllerEpics {

  private val ConfigTimeout: FiniteDuration = FiniteDuration(400, SECONDS)
  private val DefaultTimeout: FiniteDuration = FiniteDuration(60, SECONDS)

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

  def apply[F[_]: Async: Timer: Logger](epicsSys: => GsaoiEpics[F]): GsaoiFullHandler[F] = new GsaoiFullHandler[F] {
    private val L: Logger[F] = Logger[F]

    override def applyConfig(config: GsaoiConfig): F[Unit] = retrieveConfig.flatMap { current =>
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

      val guideOff: F[Unit] = (
        epicsSys.endGuideCmd.mark *>
          epicsSys.endGuideCmd.post(DefaultTimeout).void *>
          epicsSys.waitForGuideOff
      ).whenA(current.guiding)

      val guideOn: F[Unit] = (
        epicsSys.guideCmd.mark *>
          epicsSys.guideCmd.post(DefaultTimeout) *>
          epicsSys.waitForGuideOn
      ).whenA(current.guiding)

      L.debug("Start Gsaoi configuration") *>
        L.debug(s"Gsaoi configuration: ${config.show}") *>
        guideOff.whenA(ccParams.nonEmpty || dcParams.nonEmpty) *>
        ( ccParams.sequence *>
          epicsSys.ccConfigCmd.post(ConfigTimeout).void
        ).unlessA(ccParams.isEmpty) *>
        ( dcParams.sequence *>
          epicsSys.dcConfigCmd.post(ConfigTimeout).void
        ).unlessA(dcParams.isEmpty) *>
        guideOn.whenA(ccParams.nonEmpty || dcParams.nonEmpty) *>
        L.debug("Completed Gsaoi configuration")
    }

    override def observe(fileId: ImageFileId, cfg: GsaoiController.DCConfig): F[ObserveCommandResult] = {
      val checkDhs: F[Unit] = failUnlessM[F](
        epicsSys.dhsConnected.map(_ === DhsConnected.Yes),
        SeqexecFailure.Execution("GSAOI is not connected to DHS")
      )

      L.debug(s"Start GSAOI observe, file id $fileId") *>
        checkDhs *>
        epicsSys.observeCmd.setLabel(fileId) *>
        epicsSys.observeCmd.post(calcObserveTimeout(cfg)).flatTap{ _ => L.debug("Completed GSAOI observe") }
    }

    // GSAOI endObserve is a NOP with no CAR associated
    override def endObserve: F[Unit] =
      L.debug("endObserve for GSAOI skipped")

    override def stopObserve: F[Unit] =
      L.debug("Stop GSAOI exposure") *>
        epicsSys.stopCmd.mark *>
        epicsSys.stopCmd.post(DefaultTimeout).void

    override def abortObserve: F[Unit] =
      L.debug("Abort GSAOI exposure") *>
        epicsSys.abortCmd.mark *>
        epicsSys.abortCmd.post(DefaultTimeout).void

    override def observeProgress(total: Time): fs2.Stream[F, Progress] = {
      val rem = for {
        remTime    <- epicsSys.countdown.map(_.seconds)
        coaddsDone <- epicsSys.coaddsDone
        coadds     <- epicsSys.coadds
        expTime    <- epicsSys.requestedExposureTime.map(_.seconds)
      } yield (coaddsDone<coadds).fold(coadds-coaddsDone-1, 0)*expTime + remTime

      EpicsUtil.countdown[F](
        total,
        rem,
        epicsSys.observeState.widen[CarStateGeneric],
        (epicsSys.dcIsPreparing, epicsSys.dcIsAcquiring, epicsSys.dcIsReadingOut).mapN(ObserveStage.fromBooleans),
        EpicsUtil.defaultProgress[F])
    }

    def calcObserveTimeout(cfg: DCConfig): FiniteDuration = {
      val factor = 2.2
      val overhead = 300.seconds

      FiniteDuration((cfg.exposureTime * cfg.coadds.toDouble * factor + overhead).toMillis, MILLISECONDS)
    }

    private def retrieveConfig: F[EpicsGsaoiConfig] = for{
      fl <- epicsSys.filter
      uw <- epicsSys.utilWheel
      wc <- epicsSys.windowCover
      rm <- epicsSys.readMode
      ro <- epicsSys.roi
      co <- epicsSys.coadds
      et <- epicsSys.requestedExposureTime
      fo <- epicsSys.numberOfFowlerSamples
      gd <- epicsSys.guiding
    } yield EpicsGsaoiConfig(fl, uw, wc, rm, ro, co, et, fo, gd)

    override def currentState: F[GsaoiGuider.GuideState] = for {
      guide <- epicsSys.guiding
      m1    <- epicsSys.odgw1Multiplier
      m2    <- epicsSys.odgw1Multiplier
      m3    <- epicsSys.odgw1Multiplier
      m4    <- epicsSys.odgw1Multiplier
    } yield new GsaoiGuider.GuideState {
      override def isGuideActive: Boolean = guide

      override def isOdgwGuiding(odgwId: GsaoiGuider.OdgwId): Boolean = {
        import GsaoiGuider.OdgwId._
        odgwId match {
          case Odgw1 => guide && m1 > 0
          case Odgw2 => guide && m2 > 0
          case Odgw3 => guide && m3 > 0
          case Odgw4 => guide && m4 > 0
        }
      }
    }

    override def guide: F[Unit] = epicsSys.guideCmd.mark *> epicsSys.guideCmd.post(DefaultTimeout).void

    override def endGuide: F[Unit] = epicsSys.endGuideCmd.mark *> epicsSys.guideCmd.post(DefaultTimeout).void
  }
}
