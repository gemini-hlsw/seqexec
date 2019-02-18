// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.Monad
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
import edu.gemini.seqexec.server.nifs.{ ReadMode => EReadMode }
import edu.gemini.seqexec.server.nifs.{ TimeMode => ETimeMode }
import mouse.boolean._
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

  val readModeLUT: Map[LegacyReadMode, EReadMode] = Map(
    LegacyReadMode.BRIGHT_OBJECT_SPEC -> EReadMode.FOWLER,
    LegacyReadMode.MEDIUM_OBJECT_SPEC -> EReadMode.FOWLER,
    LegacyReadMode.FAINT_OBJECT_SPEC  -> EReadMode.FOWLER
  )

  val engineeringReadModeLUT: Map[LegacyEngReadMode, EReadMode] = Map(
    LegacyEngReadMode.FOWLER_SAMPLING_READOUT -> EReadMode.FOWLER,
    LegacyEngReadMode.LINEAR_READ             -> EReadMode.LINEAR
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
      case LegacyDisperser.J       => "J"
      case LegacyDisperser.H       => "H"
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

  private val ConfigTimeout: Time  = Seconds(400)
  private val DefaultTimeout: Time = Seconds(60)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  // This method takes a list of actions returning possible actions
  // If at least one is defined it will execute them and then execut after
  private def executeIfNeeded[F[_]: Monad, A](i:     List[F[Option[F[Unit]]]],
                                              after: F[A]): F[Unit] =
    i.sequence.flatMap { l =>
      val act: List[F[Unit]] = l.collect {
        case Some(x) => x
      }
      (act.sequence *> after).whenA(act.nonEmpty)
    }

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
      _ <- OptionT.liftF(epicsSys.dcConfigCmd.setTimeMode(ETimeMode.ExposureTime))
    } yield ()).value.void

  private def setExposureTime(expTime: ExposureTime): IO[Unit] =
    epicsSys.dcConfigCmd.setExposureTime(expTime.toSeconds) *>
      epicsSys.dcConfigCmd.setPeriod(1) *>
      epicsSys.dcConfigCmd.setTimeMode(ETimeMode.ReadPeriod)

  private def configDC(cfg: DCConfig): IO[Unit] =
    setReadMode(cfg.readMode) *>
      setNumberOfSamples(cfg.numberOfSamples, numberOfFowSamples(cfg.readMode)) *>
      setPeriod(cfg.period) *>
      setExposureTime(cfg.exposureTime).whenA(cfg.period.isEmpty) *>
      setCoadds(cfg.coadds) *>
      setNumberOfResets(cfg.numberOfResets) *>
      setNumberOfPeriods(cfg.numberOfPeriods)

  private def setFilter(cfg: CCConfig): IO[Option[IO[Unit]]] = {
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

  private def setImagingMirror(cfg: CCConfig): IO[Option[IO[Unit]]] = {
    val im = cfg.imagingMirror.displayValue
    smartSetParamF(im,
                   epicsSys.imagingMirror,
                   epicsSys.ccConfigCmd.setImagingMirror(im))
  }

  private def setWindowCover(cfg: CCConfig): IO[Option[IO[Unit]]] = {
    val wc = encode(cfg.windowCover)
    smartSetParamF(wc,
                   epicsSys.windowCover,
                   epicsSys.ccConfigCmd.setWindowCover(wc))
  }

  private def setDisperser(cfg: CCConfig): IO[Option[IO[Unit]]] = {
    val disperser = encode(cfg.disperser)

    val set = epicsSys.ccConfigCmd.setDisperser(disperser)

    // Here we can assume that we are actually in the right position but have been offset if:
    // * The current position is INVALID
    // * The last selected position (which was validated) is the same as where we are demanded to go
    // * The the current central wavelenght is not the same (to within tolerance) to what the default is
    // for this positoin (if it is, we should not be in a INVALID position). (NOT CHECKING FOR THIS YET).
    // So if any of those conditions is not true; need to move the disperser to the new position.
    def checkInvalid(current: Option[String]): IO[Option[IO[Unit]]] =
      epicsSys.lastSelectedDisperser.map { lsd =>
        (current.exists(_ === "INVALID") && lsd.exists(_ === disperser))
          .option(set)
      }

    // We need an even smarter set param
    for {
      instDisp     <- epicsSys.disperser
      setIfInvalid <- checkInvalid(instDisp)
    } yield if (instDisp.exists(_ =!= disperser)) set.some else setIfInvalid
  }

  private def setMask(cfg: CCConfig): IO[Option[IO[Unit]]] = {
    val mask = encode(cfg.mask)

    val set = epicsSys.ccConfigCmd.setMask(mask)

    // Here we can assume that we are actually in the right position but have been offset if:
    // * The current position is INVALID
    // * The last selected position (which was validated) is the same as where we are demanded to go
    // * The the current offset is not  0 (if it was then, the current position should not be INVALID.
    // So if any of those conditions is not true; need to move the mask to the new position.
    def checkInvalid(current: Option[String]): IO[Option[IO[Unit]]] =
      (epicsSys.maskOffset, epicsSys.lastSelectedMask).mapN { (mo, lsm) =>
        (current.exists(_ === "INVALID") && lsm.exists(_ === mask) && mo.exists(
          _ =!= 0.0)).option(set)
      }

    // We need an even smarter set param
    for {
      instMask     <- epicsSys.mask
      setIfInvalid <- checkInvalid(instMask)
    } yield if (instMask.exists(_ =!= mask)) set.some else setIfInvalid
  }

  private def firstCCPass(cfg: CCConfig): IO[Unit] =
    executeIfNeeded(List(setFilter(cfg),
                         setImagingMirror(cfg),
                         setDisperser(cfg),
                         setWindowCover(cfg),
                         setMask(cfg)),
                    postCcConfig)

  // When calculating how different the current pos is from demanded, want to check if have
  // things within some small delta. Technically this delta should be of the order of 1ustep.
  // We don't check for the equality in microns/arcsecs, because the demands may differ in microns/arcsecs,
  // but when rounded come up to the same position in usteps, and hence there is no need to move
  // the mechanism.
  private val CentralWavelengthTolerance = 0.002
  private val MaskOffsetTolerance        = 0.002

  private def setCentralWavelength(cfg: CCConfig): IO[Option[IO[Unit]]] =
    epicsSys.centralWavelength.map { curCw =>
      ((cfg.disperser =!= LegacyDisperser.MIRROR) && curCw.exists(cw =>
        abs(cw - cfg.wavelength) > CentralWavelengthTolerance)).option {
        epicsSys.ccConfigCmd
          .setCentralWavelength(f"${cfg.wavelength}1.6f")
      }
    }

  private def setMaskOffset(cfg: CCConfig): IO[Option[IO[Unit]]] =
    for {
      curMo <- epicsSys.maskOffset
    } yield
      (curMo
        .exists(mo => abs(mo - cfg.maskOffset) > MaskOffsetTolerance))
        .option {
          epicsSys.ccConfigCmd.setMaskOffset(f"${cfg.maskOffset}1.6f")
        }

  private val postCcConfig =
    epicsSys.ccConfigCmd.setTimeout[IO](ConfigTimeout) *>
      epicsSys.ccConfigCmd.post[IO]

  private def secondCCPass(cfg: CCConfig): IO[Unit] =
    executeIfNeeded(List(setCentralWavelength(cfg), setMaskOffset(cfg)),
                    postCcConfig)

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

}
