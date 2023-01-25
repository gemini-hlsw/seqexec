// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.FiniteDuration
import cats._
import cats.effect.Async
import cats.effect.Sync
import cats.kernel.Eq
import cats.syntax.all._
import edu.gemini.epics.acm.CarStateGEM5
import edu.gemini.seqexec.server.altair.LgsSfoControl
import org.typelevel.log4cats.Logger
import monocle.macros.Lenses
import mouse.boolean._
import seqexec.model.`enum`.Instrument
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.SeqexecFailure
import seqexec.server.altair.AltairController._
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import seqexec.server.tcs.Gaos.PauseCondition.{ GaosGuideOff, OiOff, P1Off }
import seqexec.server.tcs.Gaos.ResumeCondition.{ GaosGuideOn, OiOn, P1On }
import seqexec.server.tcs.Gaos._
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import seqexec.server.tcs.TcsEpics
import squants.Length
import squants.Time
import squants.space.{ Arcseconds, Millimeters }
import squants.time.TimeConversions._

object AltairControllerEpics {
  @Lenses
  final case class EpicsAltairConfig(
    currentMatrixCoords:  (Length, Length),
    preparedMatrixCoords: (Length, Length),
    strapRTStatus:        Boolean,
    strapTempStatus:      Boolean,
    stapHVoltStatus:      Boolean,
    strapLoop:            Boolean,
    sfoLoop:              LgsSfoControl,
    strapGate:            Int,
    aoLoop:               Boolean
  )

  def apply[F[_]: Async](
    epicsAltair: => AltairEpics[F],
    epicsTcs:    => TcsEpics[F]
  )(implicit L: Logger[F]): AltairController[F] = new AltairController[F] {

    private def inRangeLinear[T <: Ordered[T]](vMin: T, vMax: T)(v: T): Boolean =
      v > vMin && v < vMax

    private def newPosition(starPos: (Length, Length))(offset: FocalPlaneOffset): (Length, Length) =
      starPos.bimap(_ + offset.x, _ + offset.y)

    val CorrectionsOn: String  = "ON"
    val CorrectionsOff: String = "OFF"

    // The OT checks this, why do it again in Seqexec?
    private def newPosInRange(newPos: (Length, Length)): Boolean = {
      val minX = Millimeters(-37.2)
      val maxX = Millimeters(37.2)
      val minY = Millimeters(-37.2)
      val maxY = Millimeters(37.2)

      newPos match {
        case (x, y) => inRangeLinear(minX, maxX)(x) && inRangeLinear(minY, maxY)(y)
      }
    }

    private def validControlMatrix(mtxPos: (Length, Length))(newPos: (Length, Length)): Boolean = {
      val limit = Arcseconds(5.0) / FOCAL_PLANE_SCALE

      val diff = newPos.bimap(_ - mtxPos._1, _ - mtxPos._2)

      diff._1 * diff._1 + diff._2 * diff._2 < limit * limit
    }

    private def validateCurrentControlMatrix(
      currCfg: EpicsAltairConfig,
      newPos:  (Length, Length)
    ): Boolean = validControlMatrix(currCfg.currentMatrixCoords)(newPos)

    private def validatePreparedControlMatrix(
      currCfg: EpicsAltairConfig,
      newPos:  (Length, Length)
    ): Boolean = validControlMatrix(currCfg.preparedMatrixCoords)(newPos)

    private def prepareMatrix(newPos: (Length, Length)): F[Unit] =
      epicsTcs.aoPrepareControlMatrix.setX(newPos._1.toMillimeters) *>
        epicsTcs.aoPrepareControlMatrix.setY(newPos._2.toMillimeters)

    private def pauseNgsMode(
      position:   (Length, Length),
      currCfg:    EpicsAltairConfig,
      instrument: Instrument
    )(pauseReasons: PauseConditionSet): PauseReturn[F] = {
      // There are two reasons to stop NGS:
      // 1. This is an unguided step
      // 2. The current control matrix will not be valid for the end position after applying an offset (i.e. the offset
      // is too big).

      val guidedStep    = !pauseReasons.contains(PauseCondition.GaosGuideOff)
      val currMatrixOk  = validateCurrentControlMatrix(currCfg, position)
      val prepMatrixOk  = validatePreparedControlMatrix(currCfg, position)
      val isSmallOffset = pauseReasons.offsetO.forall(canGuideWhileOffseting(_, instrument))

      val needsToStop = !guidedStep || !currMatrixOk || !isSmallOffset

      val mustPrepareMatrix = (!currCfg.aoLoop || needsToStop) && guidedStep && !prepMatrixOk

      // Prepare action will mark prepare the command but not run it. That will be triggered as part of the TCS
      // configuration
      val configActions = mustPrepareMatrix.option(prepareMatrix(position))

      val pauseAction =
        L.debug(
          s"Pausing Altair NGS guiding because guidedStep=$guidedStep, currMatrixOk=$currMatrixOk"
        ) *>
          setCorrectionsOff *>
          L.debug("Altair guiding NGS paused") *>
          (L.debug("Flatting Altair DM") *> dmFlattenAction).whenA(!guidedStep)

      if (currCfg.aoLoop && needsToStop)
        PauseReturn[F](
          wasPaused = true,
          pauseAction.some,
          pauseTargetFilter =
            pauseReasons.offsetO.nonEmpty,                                        // if not guiding, pause target filter for offsets
          GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
          configActions
        )
      else
        PauseReturn[F](
          wasPaused = false,
          L.debug(
            s"Skipped pausing Altair NGS guiding because currCfg.aoLoop=${currCfg.aoLoop}, guidedStep=$guidedStep, isSmallOffset = $isSmallOffset, currMatrixOk=$currMatrixOk"
          ).some,
          pauseTargetFilter =
            (!currCfg.aoLoop && pauseReasons.offsetO.nonEmpty) || !isSmallOffset, // pause target filter for unguided offset
          GuideCapabilities(canGuideM2 = currCfg.aoLoop, canGuideM1 = currCfg.aoLoop),
          configActions
        )

    }

    private val setCorrectionsOff: F[ApplyCommandResult] =
      epicsTcs.aoCorrect.setCorrections(CorrectionsOff) *> epicsTcs.aoCorrect.post(DefaultTimeout)

    private val setCorrectionsOn: F[ApplyCommandResult] =
      epicsTcs.aoCorrect.setCorrections(CorrectionsOn) *>
        epicsTcs.aoCorrect.setGains(1) *>
        epicsTcs.aoCorrect.post(DefaultTimeout)

    private def pauseResumeNgsMode(
      startPos:   (Length, Length),
      currCfg:    EpicsAltairConfig,
      currOffset: FocalPlaneOffset,
      instrument: Instrument
    )(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet): AltairPauseResume[F] = {
      val newPos                = pauseReasons.offsetO
        .map(x => newPosition(startPos)(x.to))
        .getOrElse(newPosition(startPos)(currOffset))
      val forceFreeze           = !newPosInRange(newPos)
      val adjustedPauseReasons  =
        forceFreeze.fold(pauseReasons + PauseCondition.GaosGuideOff, pauseReasons)
      val adjustedResumeReasons =
        forceFreeze.fold(resumeReasons - ResumeCondition.GaosGuideOn, resumeReasons)

      val pauseResult = pauseNgsMode(newPos, currCfg, instrument)(adjustedPauseReasons)
      val resume      = resumeNgsMode(
        currCfg.aoLoop,
        currCfg.aoLoop && pauseResult.wasPaused,
        adjustedResumeReasons
      )

      AltairPauseResume(
        pauseResult.pauseAction,
        pauseResult.keepGuiding,
        pauseResult.pauseTargetFilter,
        resume.resumeAction,
        resume.keepGuiding,
        pauseResult.config,
        forceFreeze
      )
    }

    private val AoSettledTimeout  = FiniteDuration(30, SECONDS)
    private val MatrixPrepTimeout = FiniteDuration(10, SECONDS)

    private val dmFlattenAction: F[ApplyCommandResult] =
      epicsTcs.aoFlatten.mark *> epicsTcs.aoFlatten.post(DefaultTimeout)

    // Let's keep this check until we are sure the coordinates for the control matrix are properly estimated
    private val checkControlMatrix: F[Unit] = {
      val tolerance = Millimeters(1e-3)

      for {
        pmtxx <- epicsTcs.aoPreparedCMX.map(Millimeters(_))
        pmtxy <- epicsTcs.aoPreparedCMY.map(Millimeters(_))
        aogsx <- epicsTcs.aoGuideStarX.map(Millimeters(_))
        aogsy <- epicsTcs.aoGuideStarY.map(Millimeters(_))
        _     <-
          L.warn(
            s"Altair prepared matrix coordinates ($pmtxx, $pmtxy) don't match guide star coordinates ($aogsx, $aogsy)"
          ).whenA(
            (pmtxx - aogsx) * (pmtxx - aogsx) + (pmtxy - aogsy) * (pmtxy - aogsy) > tolerance * tolerance
          )
      } yield ()
    }

    private def resumeNgsMode(
      aoOn:      Boolean,
      wasPaused: Boolean,
      reasons:   ResumeConditionSet
    ): ResumeReturn[F] = {
      val guidedStep = reasons.contains(ResumeCondition.GaosGuideOn)

      val action = if ((aoOn && !wasPaused) || !guidedStep)
        L.debug(
          s"Skipped resuming Altair NGS guiding because wasPaused=$wasPaused, guidedStep=$guidedStep"
        )
      else
        L.debug(s"Resume Altair NGS guiding because guidedStep=$guidedStep") *>
          epicsAltair.controlMatrixCalc.flatMap { x =>
            (L.debug("Altair control matrix calculation not yet ready, waiting for it") *>
              epicsAltair.waitMatrixCalc(CarStateGEM5.IDLE, MatrixPrepTimeout)).whenA(x.isBusy)
          } *>
          checkControlMatrix *>
          setCorrectionsOn *>
          L.debug("Altair NGS guiding resumed, waiting for it ti settle") *>
          epicsAltair.waitAoSettled(AoSettledTimeout) *>
          L.debug("Altair NGS guiding settled")

      ResumeReturn(
        action.some,
        GuideCapabilities(canGuideM2 = reasons.contains(ResumeCondition.GaosGuideOn),
                          canGuideM1 = reasons.contains(ResumeCondition.GaosGuideOn)
        )
      )
    }

    private def checkStrapLoopState(currCfg: EpicsAltairConfig): Either[SeqexecFailure, Unit] =
      currCfg.strapRTStatus.either(
        SeqexecFailure.Unexpected("Cannot start Altair STRAP loop, RT Control status is bad."),
        ()
      ) *>
        currCfg.strapTempStatus.either(
          SeqexecFailure.Unexpected(
            "Cannot start Altair STRAP loop, Temperature Control status is bad."
          ),
          ()
        ) *>
        currCfg.stapHVoltStatus.either(
          SeqexecFailure.Unexpected("Cannot start Altair STRAP loop, HVolt status is bad."),
          ()
        )

    private def startStrapGate(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Starting STRAP gate in Altair") *>
        epicsAltair.strapGateControl.setGate(1) *>
        epicsAltair.strapGateControl.post(DefaultTimeout) *>
        epicsAltair.waitForStrapGate(100, StrapGateTimeout) *>
        L.debug("STRAP gate started")
    ).unlessA(currCfg.strapGate =!= 0)

    private def stopStrapGate(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Stopping STRAP gate in Altair") *>
        epicsAltair.strapGateControl.setGate(0) *>
        epicsAltair.strapGateControl.post(DefaultTimeout) *>
        L.debug("STRAP gate stopped")
    ).whenA(currCfg.strapGate =!= 0)

    private val StrapLoopSettleTimeout = FiniteDuration(10, SECONDS)

    private def startStrapLoop(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Starting STRAP loop in Altair") *>
        epicsAltair.strapControl.setActive(1) *>
        epicsAltair.strapControl.post(DefaultTimeout) *>
        epicsAltair.waitForStrapLoop(v = true, StrapLoopSettleTimeout) *>
        L.debug("STRAP loop started")
    ).unlessA(currCfg.strapLoop)

    private def stopStrapLoop(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Stopping STRAP loop in Altair") *>
        epicsAltair.strapControl.setActive(0) *>
        epicsAltair.strapControl.post(DefaultTimeout) *>
        L.debug("STRAP loop stopped")
    ).whenA(currCfg.strapLoop)

    implicit val sfoControlEq: Eq[LgsSfoControl] = Eq.by(_.ordinal)

    private def startSfoLoop(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Start SFO loop in Altair") *>
        epicsAltair.sfoControl.setActive(LgsSfoControl.Enable) *>
        epicsAltair.sfoControl.post(DefaultTimeout) *>
        L.debug("SFO loop started")
    ).unlessA(currCfg.sfoLoop === LgsSfoControl.Enable)

    private def pauseSfoLoop(currCfg: EpicsAltairConfig): F[Unit] = (
      L.debug("Pause SFO loop in Altair") *>
        epicsAltair.sfoControl.setActive(LgsSfoControl.Pause) *>
        epicsAltair.sfoControl.post(DefaultTimeout) *>
        L.debug("SFO loop paused")
    ).whenA(currCfg.sfoLoop === LgsSfoControl.Enable)

    private def ttgsOn(strap: Boolean, sfo: Boolean, currCfg: EpicsAltairConfig): F[Unit] =
      checkStrapLoopState(currCfg).fold(ApplicativeError[F, Throwable].raiseError,
                                        Sync[F].delay(_)
      ) *>
        (startStrapGate(currCfg) *> startStrapLoop(currCfg)).whenA(strap) *>
        startSfoLoop(currCfg).whenA(sfo)

    private val ttgsOffEndo: Endo[EpicsAltairConfig] = EpicsAltairConfig.strapGate.replace(0) >>>
      EpicsAltairConfig.strapLoop.replace(false) >>>
      EpicsAltairConfig.sfoLoop.modify { v =>
        (v === LgsSfoControl.Disable).fold(LgsSfoControl.Disable, LgsSfoControl.Pause)
      }

    private def ttgsOff(currCfg: EpicsAltairConfig): F[Unit] =
      stopStrapGate(currCfg) *>
        stopStrapLoop(currCfg) *>
        pauseSfoLoop(currCfg)

    private def pauseResumeLgsMode(
      strap:      Boolean,
      sfo:        Boolean,
      startPos:   (Length, Length),
      currCfg:    EpicsAltairConfig,
      currOffset: FocalPlaneOffset,
      instrument: Instrument
    )(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet): AltairPauseResume[F] = {
      val newPos                = pauseReasons.offsetO
        .map(x => newPosition(startPos)(x.to))
        .getOrElse(newPosition(startPos)(currOffset))
      val forceFreeze           = !newPosInRange(newPos)
      val adjustedPauseReasons  =
        forceFreeze.fold(pauseReasons + PauseCondition.GaosGuideOff, pauseReasons)
      val adjustedResumeReasons =
        forceFreeze.fold(resumeReasons - ResumeCondition.GaosGuideOn, resumeReasons)

      val pause      = pauseLgsMode(strap, sfo, currCfg, instrument)(adjustedPauseReasons)
      val updatedCfg = pause.wasPaused.fold(ttgsOffEndo(currCfg), currCfg)
      val resume     = resumeLgsMode(strap, sfo, updatedCfg, adjustedResumeReasons)

      AltairPauseResume(
        pause.pauseAction,
        pause.keepGuiding,
        pause.pauseTargetFilter,
        resume.resumeAction,
        resume.keepGuiding,
        none,
        forceFreeze
      )
    }

    private def pauseLgsMode(
      strap:      Boolean,
      sfo:        Boolean,
      currCfg:    EpicsAltairConfig,
      instrument: Instrument
    )(reasons: PauseConditionSet): PauseReturn[F] = {
      val guidedStep    = !reasons.contains(PauseCondition.GaosGuideOff)
      val isSmallOffset = reasons.offsetO.forall(canGuideWhileOffseting(_, instrument))
      val mustPauseNGS  = !(guidedStep && isSmallOffset) && (strap || sfo)
      val usingNGS      = currCfg.sfoLoop === LgsSfoControl.Enable || currCfg.strapLoop

      val pauseAction = L.debug(
        s"Pausing Altair LGS(strap = $strap, sfo = $sfo) guiding because guidedStep=$guidedStep, isSmallOffset=$isSmallOffset"
      ) *>
        ttgsOff(currCfg) *>
        setCorrectionsOff *>
        L.debug(s"Altair LGS(strap = $strap, sfo = $sfo) guiding paused") *>
        (L.debug("Flatting Altair DM") *> dmFlattenAction).whenA(!guidedStep)

      if (usingNGS && mustPauseNGS)
        PauseReturn(
          wasPaused = true,
          pauseAction.some,
          pauseTargetFilter = reasons.offsetO.nonEmpty,
          GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
          none
        )
      else
        PauseReturn(
          wasPaused = false,
          L.debug(
            s"Skipped pausing Altair LGS(strap = $strap, sfo = $sfo) guiding, guidedStep=$guidedStep, isSmallOffset=$isSmallOffset"
          ).some,
          pauseTargetFilter = (!usingNGS && reasons.offsetO.nonEmpty) || !isSmallOffset,
          GuideCapabilities(canGuideM2 =
                              currCfg.sfoLoop === LgsSfoControl.Enable || currCfg.strapLoop,
                            canGuideM1 = true
          ),
          none
        )

    }

    private def resumeLgsMode(
      strap:      Boolean,
      sfo:        Boolean,
      currentCfg: EpicsAltairConfig,
      reasons:    ResumeConditionSet
    ): ResumeReturn[F] = {
      val guidedStep   = reasons.contains(ResumeCondition.GaosGuideOn)
      val alreadyThere =
        (currentCfg.sfoLoop === LgsSfoControl.Enable && sfo) && (currentCfg.strapLoop && strap)

      val action = if (!alreadyThere && guidedStep)
        L.debug(
          s"Resuming Altair LGS(strap = $strap, sfo = $sfo) guiding because guidedStep=$guidedStep"
        ) *>
          setCorrectionsOn *>
          ttgsOn(strap, sfo, currentCfg) *>
          L.debug(s"Altair LGS(strap = $strap, sfo = $sfo) guiding resumed")
      else
        L.debug(s"Skipped resuming Altair LGS(strap = $strap, sfo = $sfo) guiding")

      ResumeReturn(
        action.some,
        GuideCapabilities(
          canGuideM2 = (strap || sfo) && guidedStep,
          canGuideM1 = (strap || sfo) && guidedStep
        )
      )
    }

    private def turnOff(currentCfg: EpicsAltairConfig): AltairPauseResume[F] = {
      val pauseAction =
        if (currentCfg.aoLoop)
          L.debug("Turning Altair guiding off") *>
            setCorrectionsOff *>
            L.debug("Altair guiding turned off")
        else
          L.debug("Skipped turning Altair guiding off")

      AltairPauseResume(
        pauseAction.some,
        GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
        pauseTargetFilter = true,
        none,
        GuideCapabilities(canGuideM2 = false, canGuideM1 = false),
        none,
        forceFreeze = true
      )
    }

    override def pauseResume(
      pauseReasons:  PauseConditionSet,
      resumeReasons: ResumeConditionSet,
      currentOffset: FocalPlaneOffset,
      instrument:    Instrument
    )(cfg: AltairConfig): F[AltairPauseResume[F]] = {
      val unguidedStep                                = pauseReasons.contains(GaosGuideOff)
      def guideOff(turnOff: Boolean): Option[F[Unit]] =
        (turnOff || unguidedStep).option(
          L.debug(
            s"Pausing Altair guiding"
          ) *>
            setCorrectionsOff *>
            L.debug("Altair guiding paused") *>
            (L.debug("Flatting Altair DM") *> dmFlattenAction).whenA(unguidedStep).void
        )

      retrieveConfig.map { currCfg =>
        cfg match {
          case Ngs(_, starPos)    =>
            pauseResumeNgsMode(starPos, currCfg, currentOffset, instrument)(pauseReasons,
                                                                            resumeReasons
            )
          case Lgs(str, sfo, pos) =>
            pauseResumeLgsMode(str, sfo, pos, currCfg, currentOffset, instrument)(pauseReasons,
                                                                                  resumeReasons
            )
          case LgsWithP1          =>
            AltairPauseResume(
              guideOff(currCfg.aoLoop && pauseReasons.fixed.contains(P1Off)),
              GuideCapabilities(!pauseReasons.fixed.contains(P1Off),
                                canGuideM1 = !pauseReasons.fixed.contains(P1Off)
              ),
              pauseTargetFilter = false,
              (resumeReasons.contains(P1On) && resumeReasons.contains(
                GaosGuideOn
              ) && (!currCfg.aoLoop || pauseReasons
                .contains(P1Off))).option(setCorrectionsOn.void),
              GuideCapabilities(resumeReasons.fixed.contains(P1On),
                                canGuideM1 = resumeReasons.fixed.contains(P1On)
              ),
              none,
              forceFreeze = false
            )
          case LgsWithOi          =>
            AltairPauseResume(
              guideOff(currCfg.aoLoop && pauseReasons.fixed.contains(OiOff)),
              GuideCapabilities(!pauseReasons.fixed.contains(OiOff),
                                canGuideM1 = !pauseReasons.fixed.contains(OiOff)
              ),
              pauseTargetFilter = false,
              (resumeReasons.contains(OiOn) && resumeReasons.contains(
                GaosGuideOn
              ) && (!currCfg.aoLoop || pauseReasons
                .contains(OiOff))).option(setCorrectionsOn.void),
              GuideCapabilities(resumeReasons.fixed.contains(OiOn),
                                canGuideM1 = resumeReasons.fixed.contains(OiOn)
              ),
              none,
              forceFreeze = false
            )
          case AltairOff          => turnOff(currCfg)
        }
      }
    }

    override def observe(expTime: Time)(cfg: AltairConfig): F[Unit] = Sync[F]
      .delay(LocalDate.now)
      .flatMap(date =>
        (epicsTcs.aoStatistics.setTriggerTimeInterval(0.0) *>
          epicsTcs.aoStatistics.setInterval(expTime.toSeconds) *>
          epicsTcs.aoStatistics.setSamples(1) *>
          epicsTcs.aoStatistics.setFileName(
            "aostats" + date.format(DateTimeFormatter.ofPattern("yyyyMMdd"))
          )).whenA(expTime > 5.seconds && cfg =!= AltairOff)
      )

    override def endObserve(cfg: AltairConfig): F[Unit] = Applicative[F].unit

    def retrieveConfig: F[EpicsAltairConfig] = for {
      cmtxx <- epicsAltair.matrixStartX.map(Millimeters(_))
      cmtxy <- epicsAltair.matrixStartY.map(Millimeters(_))
      pmtxx <- epicsTcs.aoPreparedCMX.map(Millimeters(_))
      pmtxy <- epicsTcs.aoPreparedCMY.map(Millimeters(_))
      strRT <- epicsAltair.strapRTStatus
      strTm <- epicsAltair.strapTempStatus
      strHV <- epicsAltair.strapHVStatus
      strap <- epicsAltair.strapLoop
      sfo   <- epicsAltair.sfoLoop
      stGat <- epicsAltair.strapGate
      aolp  <- epicsAltair.aoLoop
    } yield EpicsAltairConfig(
      (cmtxx, cmtxy),
      (pmtxx, pmtxy),
      strRT,
      strTm,
      strHV,
      strap,
      sfo,
      stGat,
      aolp
    )

    // This is a bit convoluted. AO follow state is read from Altair, but set as part of TCS configuration
    override def isFollowing: F[Boolean] = epicsAltair.aoFollow

    // Can keep guiding while applying an offset?
    private def canGuideWhileOffseting(
      offset: PauseCondition.OffsetMove,
      inst:   Instrument
    ): Boolean = {
      val deltas = (offset.to.x - offset.from.x, offset.to.y - offset.from.y)
      aoOffsetThreshold(inst).exists(h => deltas._1 * deltas._1 + deltas._2 * deltas._2 < h * h)
    }

  }

  private def aoOffsetThreshold(instrument: Instrument): Option[Length] = instrument match {
    case Instrument.Nifs  => (Arcseconds(0.01) / FOCAL_PLANE_SCALE).some
    case Instrument.Niri  => (Arcseconds(3.0) / FOCAL_PLANE_SCALE).some
    case Instrument.Gnirs => (Arcseconds(3.0) / FOCAL_PLANE_SCALE).some
    case _                => none
  }

  // Auxiliary class that contains all the information from a pause calculation
  private sealed case class PauseReturn[F[_]](
    wasPaused:         Boolean,           // Flag for the resume calculation
    pauseAction:       Option[F[Unit]],   // The pause action
    pauseTargetFilter: Boolean,           // Does the target filter need to be disabled ?(info for TCS configuration)
    keepGuiding:       GuideCapabilities, // What guiding to keep enabled between the pause and resume
    config:            Option[F[Unit]]    // Optional Altair configuration (to run as part of TCS configuration)
  )

  // Auxiliary class that contains all the information from a resume calculation
  private sealed case class ResumeReturn[F[_]](
    resumeAction: Option[F[Unit]],  // The resume action
    keepGuiding:  GuideCapabilities // What guiding to enable after resume
  )

  private val DefaultTimeout = FiniteDuration(10, SECONDS)

  private val StrapGateTimeout = FiniteDuration(5, SECONDS)

}
