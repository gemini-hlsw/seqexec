// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.Eq
import cats.data.OptionT
import cats.effect.IO
import cats.effect.Timer
import cats.implicits._
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ ReadMode => LegacyReadMode }
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ EngReadMode => LegacyEngReadMode }
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ Filter => LegacyFilter }
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ Disperser => LegacyDisperser }
import edu.gemini.spModel.gemini.nifs.NIFSParams.{ Mask => LegacyMask }
import edu.gemini.seqexec.server.nifs.DhsConnected
import org.log4s.getLogger
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.math.abs
import seqexec.model.dhs.ImageFileId
import seqexec.server.ObserveCommand
import seqexec.server.Progress
import seqexec.server.ProgressUtil
import seqexec.server.SeqexecFailure
import seqexec.server.failUnlessM
import seqexec.server.EpicsUtil._
import seqexec.server.EpicsCodex._
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

trait NifsEncoders {
  implicit val filterEncoder: EncodeEpicsValue[LegacyFilter, String] =
    EncodeEpicsValue {
      case LegacyFilter.SAME_AS_DISPERSER => "" // Unused
      case LegacyFilter.ZJ_FILTER         => "ZJ"
      case LegacyFilter.JH_FILTER         => "JH"
      case LegacyFilter.HK_FILTER         => "HK"
      case LegacyFilter.WIRE_GRID         => "HK+Polarizer"
      case LegacyFilter.BLOCKED           => "Blocked"
    }

  implicit val windowCoverEncoder: EncodeEpicsValue[WindowCover, String] =
    EncodeEpicsValue {
      case WindowCover.Opened => "Opened"
      case WindowCover.Closed => "Closed"
    }

  implicit val maskEncoder: EncodeEpicsValue[LegacyMask, String] =
    EncodeEpicsValue {
      case LegacyMask.CLEAR         => "3.0_Mask"
      case LegacyMask.PINHOLE       => "0.1_Hole"
      case LegacyMask.PINHOLE_ARRAY => "0.2_Hole_Array"
      case LegacyMask.SLIT          => "0.2_Slit"
      case LegacyMask.RONCHE        => "Ronchi_Screen"
      case LegacyMask.OD_1          => "0.1_Occ_Disc"
      case LegacyMask.OD_2          => "0.2_Occ_Disc"
      case LegacyMask.OD_5          => "0.5_Occ_Disc"
      case LegacyMask.KG3_ND_FILTER => "KG3_ND_Filter"
      case LegacyMask.KG5_ND_FILTER => "KG5_ND_Filter"
      case LegacyMask.BLOCKED       => "Blocked"
    }

  implicit val disperserEncoder: EncodeEpicsValue[LegacyDisperser, String] =
    EncodeEpicsValue {
      case LegacyDisperser.Z       => "Z"
      case LegacyDisperser.J       => "H"
      case LegacyDisperser.H       => "J"
      case LegacyDisperser.K       => "K"
      case LegacyDisperser.K_SHORT => "K_Short"
      case LegacyDisperser.K_LONG  => "K_Long"
      case LegacyDisperser.MIRROR  => "Mirror"
    }

  implicit val filterEq: Eq[LegacyFilter]       = Eq.by(_.displayValue)
  implicit val disperserEq: Eq[LegacyDisperser] = Eq.by(_.displayValue)

}

object NifsControllerEpics extends NifsController[IO] with NifsEncoders {
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

  private def setFilter(cfg: CCConfig): IO[Unit] = {
    val actualFilter: IO[LegacyFilter] =
      if (cfg.filter === LegacyFilter.SAME_AS_DISPERSER) {
        cfg.disperser match {
          case LegacyDisperser.Z | LegacyDisperser.J           =>
            LegacyFilter.ZJ_FILTER.pure[IO]

          case LegacyDisperser.H                               =>
            LegacyFilter.JH_FILTER.pure[IO]

          case LegacyDisperser.MIRROR | LegacyDisperser.K |
              LegacyDisperser.K_SHORT | LegacyDisperser.K_LONG =>
            LegacyFilter.HK_FILTER.pure[IO]

          case _                                               =>
            IO.raiseError(
              SeqexecFailure.InvalidOp(
                "cannot set filter based on the disperser ($disperser)"))
        }
      } else cfg.filter.pure[IO]

    actualFilter.flatMap { f =>
      val ef = encode(f)
      smartSetParamF(ef, epicsSys.filter, epicsSys.ccConfigCmd.setFilter(ef))
    }
  }

  private def setImagingMirror(cfg: CCConfig): IO[Unit] = {
    val im = cfg.imagingMirror.displayValue
    smartSetParamF(im,
                   epicsSys.imagingMirror,
                   epicsSys.ccConfigCmd.setImagingMirror(im))
  }

  private def setWindowCover(cfg: CCConfig): IO[Unit] = {
    val wc = encode(cfg.windowCover)
    smartSetParamF(wc,
                   epicsSys.windowCover,
                   epicsSys.ccConfigCmd.setWindowCover(wc))
  }

  private def setDisperser(cfg: CCConfig): IO[Unit] = {
    val disperser = encode(cfg.disperser)

    val set = epicsSys.ccConfigCmd.setDisperser(disperser)

    // Here we can assume that we are actually in the right position but have been offset if:
    // * The current position is INVALID
    // * The last selected position (which was validated) is the same as where we are demanded to go
    // * The the current central wavelenght is not the same (to within tolerance) to what the default is
    // for this positoin (if it is, we should not be in a INVALID position). (NOT CHECKING FOR THIS YET).
    // So if any of those conditions is not true; need to move the disperser to the new position.
    def checkInvalid(current: Option[String]): IO[Unit] =
      OptionT(epicsSys.lastSelectedDisperser)
        .map { lsd =>
          set.unlessA(current.exists(_ === "INVALID") && lsd === disperser)
        }
        .value
        .void

    // We need an even smarter set param
    (for {
      instDisp <- epicsSys.disperser
      different = instDisp.exists(_ =!= disperser)
    } yield if (different) set else checkInvalid(instDisp)).void
  }

  private def setMask(cfg: CCConfig): IO[Unit] = {
    val mask = encode(cfg.mask)

    val set = epicsSys.ccConfigCmd.setMask(mask)

    // Here we can assume that we are actually in the right position but have been offset if:
    // * The current position is INVALID
    // * The last selected position (which was validated) is the same as where we are demanded to go
    // * The the current offset is not  0 (if it was then, the current position should not be INVALID.
    // So if any of those conditions is not true; need to move the mask to the new position.
    def checkInvalid(current: Option[String]): IO[Unit] =
      (for {
        mo  <- OptionT(epicsSys.maskOffset)
        lsm <- OptionT(epicsSys.lastSelectedMask)
      } yield
        set.unlessA(
          current
            .exists(_ === "INVALID") && lsm === mask && mo =!= 0.0)).value.void

    // We need an even smarter set param
    (for {
      instMask <- epicsSys.mask
      different = instMask.exists(_ =!= mask)
    } yield if (different) set else checkInvalid(instMask)).void
  }

  private def firstCCPass(cfg: CCConfig): IO[Unit] =
    setFilter(cfg) *>
      setImagingMirror(cfg) *>
      setDisperser(cfg) *>
      setWindowCover(cfg) *>
      setMask(cfg)

  // When calculating how different the current pos is from demanded, want to check if have
  // things within some small delta. Technically this delta should be of the order of 1ustep.
  // We don't check for the equality in microns/arcsecs, because the demands may differ in microns/arcsecs,
  // but when rounded come up to the same position in usteps, and hence there is no need to move
  // the mechanism.
  private val CentralWavelengthTolerance = 0.002
  private val MaskOffsetTolerance        = 0.002

  private def setCentralWavelength(cfg: CCConfig): IO[Unit] =
    (for {
      curCw <- OptionT(epicsSys.centralWavelength)
      cw = curCw
      if abs(cw - cfg.wavelength) > CentralWavelengthTolerance
    } yield
      epicsSys.ccConfigCmd
        .setCentralWavelength(f"${cfg.wavelength}1.6f")
        .unlessA(cfg.disperser === LegacyDisperser.MIRROR)).value.void

  private def setMaskOffset(cfg: CCConfig): IO[Unit] =
    (for {
      curMo <- OptionT(epicsSys.maskOffset)
      mo = curMo
      if abs(mo - cfg.maskOffset) > MaskOffsetTolerance
    } yield
      epicsSys.ccConfigCmd.setMaskOffset(f"${cfg.maskOffset}1.6f")).value.void

  private def secondCCPass(cfg: CCConfig): IO[Unit] =
    setCentralWavelength(cfg) *>
      setMaskOffset(cfg)

  private def configCC(cfg: CCConfig): IO[Unit] =
    //IO.sleep(1500.millisecond) *> // the TCL seqexec does this but we'll skip for now
    firstCCPass(cfg) *>
      //IO.sleep(1500.millisecond) *> // the TCL seqexec does this but we'll skip for now
      secondCCPass(cfg)

  override def applyConfig(config: NifsController.NifsConfig): IO[Unit] =
    configCC(config.cc) *> configDC(config.dc)

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
      val MinIntTime = exp.map(Seconds(_)).getOrElse(Seconds(0))

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
