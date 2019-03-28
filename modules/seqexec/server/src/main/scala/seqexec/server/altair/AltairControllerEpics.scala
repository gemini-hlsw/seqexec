// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.Endo
import cats.effect.IO
import cats.implicits._
import cats.kernel.Eq
import edu.gemini.epics.acm.CarStateGEM5
import mouse.boolean._
import edu.gemini.seqexec.server.altair.LgsSfoControl
import edu.gemini.spModel.gemini.altair.AltairParams.FieldLens
import monocle.macros.Lenses
import seqexec.server.{SeqAction, SeqexecFailure, TrySeq}
import seqexec.server.altair.AltairController._
import seqexec.server.tcs.{FOCAL_PLANE_SCALE, Gaos, TcsEpics}
import seqexec.server.tcs.Gaos._
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.{Length, Time}
import squants.time.TimeConversions._
import squants.space.{Arcseconds, Millimeters}

object AltairControllerEpics extends AltairController[IO] {

  private val epicsAltair = AltairEpics.instance
  private val epicsTcs = TcsEpics.instance

  private def inRangeLinear[T <: Ordered[T]](vMin: T, vMax: T)(v: T): Boolean =
    v > vMin && v < vMax

  private def newPosition(starPos: (Length, Length))(next: FocalPlaneOffset): (Length, Length) =
    starPos.bimap(_ + next.x, _ + next.y)

  val CorrectionsOn: String = "ON"
  val CorrectionsOff: String = "OFF"
  val TargetFilterOpen: String = "Open"
  val TargetFilterClosed: String = "Closed"

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

  private def validateCurrentControlMatrix(currCfg: EpicsAltairConfig, newPos: (Length, Length))
  : Boolean = validControlMatrix(currCfg.currentMatrixCoords)(newPos)

  private def validatePreparedControlMatrix(currCfg: EpicsAltairConfig, newPos: (Length, Length))
  : Boolean = validControlMatrix(currCfg.preparedMatrixCoords)(newPos)

  private def prepareMatrix(newPos: (Length, Length)): IO[Unit] = convertTcsAction(
    epicsTcs.aoPrepareControlMatrix.setX(newPos._1.toMillimeters) *>
      epicsTcs.aoPrepareControlMatrix.setY(newPos._2.toMillimeters)
  )

  implicit val fieldLensEq: Eq[FieldLens] = Eq.by(_.ordinal)

  private def pauseNgsOrLgsMode(starPos: (Length, Length), fieldLens: FieldLens, currCfg: EpicsAltairConfig)(
    reasons: Set[Gaos.PauseCondition])
  : Option[(EpicsAltairConfig, IO[Unit])] = {
    val offsets = reasons.collectFirst { case OffsetMove(p, n) => (p, n) }
    val newPos = offsets.map(u => newPosition(starPos)(u._2))
    val newPosOk = newPos.forall(newPosInRange)
    val matrixOk = newPos.forall(validateCurrentControlMatrix(currCfg, _)) || fieldLens === FieldLens.IN
    val prepMatrixOk = newPos.forall(validatePreparedControlMatrix(currCfg, _)) || fieldLens === FieldLens.IN
    val guideOk = !reasons.contains(GaosGuideOff) //It can follow the guide star on this step

    val needsToStop = !(newPosOk && matrixOk && guideOk)

    // How the current configuration changes if loops are stopped
    val newCfg = (EpicsAltairConfig.preparedMatrixCoords.modify(v =>
      newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk).getOrElse(v)) >>>
      EpicsAltairConfig.aoLoop.set(!needsToStop)
    ) (currCfg)

    // Actions to stop loops
    val actions = List(
      currCfg.aoLoop.option(convertTcsAction(epicsTcs.aoCorrect.setCorrections(CorrectionsOff) *>
        epicsTcs.targetFilter.setShortCircuit(TargetFilterClosed))),
      newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk).map(prepareMatrix)
    ).collect { case Some(x) => x }

    val pause = (actions.sequence *> convertTcsAction(epicsTcs.targetFilter.post).void).whenA(actions.nonEmpty)

    needsToStop.option((newCfg, pause))
  }

  private def pauseResumeNgsMode(starPos: (Length, Length), fieldLens: FieldLens, currCfg: EpicsAltairConfig)(
    pauseReasons: Set[Gaos.PauseCondition], resumeReasons: Set[Gaos.ResumeCondition])
  : PauseResume[IO] = {
    val pause = pauseNgsOrLgsMode(starPos, fieldLens, currCfg)(pauseReasons)
    val resume = resumeNgsOrLgsMode(starPos, pause.map(_._1).getOrElse(currCfg))(resumeReasons)

    PauseResume(pause.map(_._2), resume)
  }

  val aoSettledTimeout: Time = 30.0.seconds
  val matrixPrepTimeout: Time = 10.seconds

  private def resumeNgsOrLgsMode(starPos: (Length, Length), currCfg: EpicsAltairConfig)(
    reasons: Set[Gaos.ResumeCondition]): Option[IO[Unit]] = {
    val offsets = reasons.collectFirst { case OffsetReached(o) => o }
    val newPosOk = offsets.forall(v => newPosInRange(newPosition(starPos)(v)))
    val guideOk = reasons.contains(GaosGuideOn)

    (newPosOk && guideOk).option(
      (epicsAltair.waitMatrixCalc(CarStateGEM5.IDLE, matrixPrepTimeout) *>
        convertTcsAction(
          epicsTcs.aoCorrect.setCorrections(CorrectionsOn) *>
          epicsTcs.aoFlatten.mark *>
          epicsTcs.targetFilter.setShortCircuit(TargetFilterOpen) *>
          epicsTcs.targetFilter.post
        ) *>
        epicsAltair.waitAoSettled(aoSettledTimeout)
      ).whenA(!currCfg.aoLoop)
    )
  }

  private def checkStrapLoopState(currCfg: EpicsAltairConfig): TrySeq[Unit] =
    currCfg.strapRTStatus.either(
      SeqexecFailure.Unexpected("Cannot start Altair STRAP loop, RT Control status is bad."), ()
    ) *>
      currCfg.strapTempStatus.either(
        SeqexecFailure.Unexpected("Cannot start Altair STRAP loop, Temperature Control status is bad."), ()
      ) *>
      currCfg.stapHVoltStatus.either(
        SeqexecFailure.Unexpected("Cannot start Altair STRAP loop, HVolt status is bad."), ()
      )

  private val DefaultTimeout = 10.seconds

  private def startStrapGate(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.strapGateControl.setGate(1) *>
      epicsAltair.strapGateControl.setTimeout[IO](DefaultTimeout) *>
      epicsAltair.strapGateControl.post[IO] *>
      epicsAltair.waitForStrapGate(100, 5.seconds)
    ).unlessA(currCfg.strapGate =!= 0)

  private def stopStrapGate(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.strapGateControl.setGate(0) *>
      epicsAltair.strapGateControl.setTimeout[IO](DefaultTimeout) *>
      epicsAltair.strapGateControl.post[IO].void
    ).whenA(currCfg.strapGate =!= 0)

  private def startStrapLoop(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.strapControl.setActive(1) *>
      epicsAltair.strapControl.setTimeout[IO](DefaultTimeout) *>
      epicsAltair.strapControl.post[IO] *>
      epicsAltair.waitForStrapLoop(v = true, 10.seconds)
    ).unlessA(currCfg.strapLoop)

  private def stopStrapLoop(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.strapControl.setActive(0) *>
      epicsAltair.strapControl.setTimeout[IO](DefaultTimeout) *>
      epicsAltair.strapControl.post[IO].void
    ).whenA(currCfg.strapLoop)

  implicit val sfoControlEq: Eq[LgsSfoControl] = Eq.by(_.ordinal)

  private def startSfoLoop(currCfg: EpicsAltairConfig): IO[Unit] =
    epicsAltair.sfoControl.setActive(LgsSfoControl.Enable).unlessA(currCfg.sfoLoop === LgsSfoControl.Enable)

  private def pauseSfoLoop(currCfg: EpicsAltairConfig): IO[Unit] =
    epicsAltair.sfoControl.setActive(LgsSfoControl.Pause).whenA(currCfg.sfoLoop === LgsSfoControl.Enable)

  private def ttgsOn(strap: Boolean, sfo: Boolean, currCfg: EpicsAltairConfig): IO[Unit] =
    checkStrapLoopState(currCfg).fold(IO.raiseError, IO(_)) *>
      (startStrapGate(currCfg) *> startStrapLoop(currCfg)).whenA(strap) *>
      startSfoLoop(currCfg).whenA(sfo)

  private val ttgsOffEndo: Endo[EpicsAltairConfig] = EpicsAltairConfig.strapGate.set(0) >>>
    EpicsAltairConfig.strapLoop.set(false) >>>
    EpicsAltairConfig.sfoLoop.modify { v =>
      (v === LgsSfoControl.Disable).fold(LgsSfoControl.Disable, LgsSfoControl.Pause)
    }

  private def ttgsOff(currCfg: EpicsAltairConfig): IO[Unit] =
    stopStrapGate(currCfg) *>
      stopStrapLoop(currCfg) *>
      pauseSfoLoop(currCfg)

  private def pauseResumeLgsMode(strap: Boolean, sfo: Boolean, starPos: (Length, Length), fieldLens: FieldLens,
                           currCfg: EpicsAltairConfig)(
    pauseReasons: Set[Gaos.PauseCondition], resumeReasons: Set[Gaos.ResumeCondition]): PauseResume[IO] = {
    val pause = pauseLgsMode(strap, sfo, starPos, fieldLens, currCfg)(pauseReasons)
    val resume = resumeLgsMode(strap, sfo, starPos)(pause.map(_._1).getOrElse(currCfg))(resumeReasons)

    PauseResume(pause.map(_._2), resume)
  }

  private def pauseLgsMode(strap: Boolean, sfo: Boolean, starPos: (Length, Length), fieldLens: FieldLens,
                           currCfg: EpicsAltairConfig)(reasons: Set[Gaos.PauseCondition])
  : Option[(EpicsAltairConfig, IO[Unit])] =
    pauseNgsOrLgsMode(starPos, fieldLens, currCfg)(reasons).filter(_ => strap || sfo)
      .map(_.bimap(ttgsOffEndo, ttgsOff(currCfg) *> _))

  private def resumeLgsMode(strap: Boolean, sfo: Boolean, starPos: (Length, Length))(currCfg: EpicsAltairConfig)(
    reasons: Set[Gaos.ResumeCondition]): Option[IO[Unit]] =
    resumeNgsOrLgsMode(starPos, currCfg)(reasons).filter(_ => strap || sfo).map(_ *> ttgsOn(strap, sfo, currCfg))

  /*
   * Modes LgsWithP1 and LgsWithOi don't use an Altair target. The only action required is to start or stop corrections
   */
  private def pauseResumeLgsWithXX(currCfg: EpicsAltairConfig)(
      pauseReasons: Set[Gaos.PauseCondition], resumeReasons: Set[Gaos.ResumeCondition])
  : PauseResume[IO] = {
    val pause = (pauseReasons.contains(Gaos.GaosGuideOff) && currCfg.aoLoop).option{
      convertTcsAction(epicsTcs.aoCorrect.setCorrections(CorrectionsOff) *>
        epicsTcs.targetFilter.setShortCircuit(TargetFilterClosed) *>
        epicsTcs.targetFilter.post.void
      )
    }
    val resume = (resumeReasons.contains(Gaos.GaosGuideOn) &&
      (pauseReasons.contains(Gaos.GaosGuideOff) || !currCfg.aoLoop)
    ).option{
      convertTcsAction(
        epicsTcs.aoCorrect.setCorrections(CorrectionsOn) *>
          epicsTcs.aoFlatten.mark *>
          epicsTcs.targetFilter.setShortCircuit(TargetFilterOpen) *>
          epicsTcs.targetFilter.post
      ) *>
      epicsAltair.waitAoSettled(aoSettledTimeout)
    }

    PauseResume(
      pause,
      resume
    )
  }

  private def turnOff(c: EpicsAltairConfig): PauseResume[IO] = PauseResume(
    convertTcsAction(epicsTcs.aoCorrect.setCorrections(CorrectionsOff) *>
      epicsTcs.targetFilter.post
    ).whenA(c.aoLoop).some,
    None
  )

  override def pauseResume(pauseReasons: Set[PauseCondition], resumeReasons: Set[ResumeCondition],
                           fieldLens: FieldLens)(cfg: AltairConfig): IO[PauseResume[IO]] =
    retrieveConfig.map{ currCfg =>
      cfg match {
        case Ngs(_, starPos)                      => pauseResumeNgsMode(starPos, fieldLens, currCfg)(pauseReasons,
          resumeReasons)
        case Lgs(str: Boolean, sfo: Boolean, pos) => pauseResumeLgsMode(str, sfo, pos, fieldLens, currCfg)(pauseReasons,
          resumeReasons)
        case LgsWithP1                            => pauseResumeLgsWithXX(currCfg)(pauseReasons, resumeReasons)
        case LgsWithOi                            => pauseResumeLgsWithXX(currCfg)(pauseReasons, resumeReasons)
        case AltairOff                            => turnOff(currCfg)
      }
    }

  override def observe(expTime: Time)(cfg: AltairConfig): IO[Unit] = IO(LocalDate.now).flatMap( date =>
    convertTcsAction(
      epicsTcs.aoStatistics.setTriggerTimeInterval(0.0) *>
        epicsTcs.aoStatistics.setInterval(expTime.toSeconds) *>
        epicsTcs.aoStatistics.setSamples(1) *>
        epicsTcs.aoStatistics.setFileName("aostats" + date.format(DateTimeFormatter.ofPattern("yyyyMMdd")))
    ).whenA(expTime > 5.seconds && cfg =!= AltairOff)
  )

  override def endObserve(cfg: AltairConfig): IO[Unit] = IO.unit

  private def getStatusVal[A](get: IO[Option[A]], name: String, system: String): IO[A] = get.flatMap(
    _.map(IO(_)).getOrElse(IO.raiseError(SeqexecFailure.Unexpected(s"Unable to read $name from $system.")))
  )

  def retrieveConfig: IO[EpicsAltairConfig] = for {
    cmtxx <- getStatusVal(epicsAltair.matrixStartX.map(_.map(Millimeters(_))), "current control matrix X", "Altair")
    cmtxy <- getStatusVal(epicsAltair.matrixStartY.map(_.map(Millimeters(_))), "current control matrix Y", "Altair")
    pmtxx <- getStatusVal(epicsTcs.aoPreparedCMX.map(_.map(Millimeters(_))), "Altair next control matrix X", "TCS")
    pmtxy <- getStatusVal(epicsTcs.aoPreparedCMY.map(_.map(Millimeters(_))), "Altair next control matrix Y", "TCS")
    strRT <- getStatusVal(epicsAltair.strapRTStatus, "strap RT control status", "Altair")
    strTm <- getStatusVal(epicsAltair.strapTempStatus, "strap temperature control status", "Altair")
    strHV <- getStatusVal(epicsAltair.strapHVStatus, "strap HVolt status", "Altair")
    strap <- getStatusVal(epicsAltair.strapLoop, "strap loop state", "Altair")
    sfo   <- getStatusVal(epicsAltair.sfoLoop, "SFO loop state", "Altair")
    stGat <- getStatusVal(epicsAltair.strapGate, "strap gate state", "Altair")
    aolp  <- getStatusVal(epicsAltair.aoLoop, "AO active", "Altair")
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

  @Lenses
  final case class EpicsAltairConfig(
    currentMatrixCoords: (Length, Length),
    preparedMatrixCoords: (Length, Length),
    strapRTStatus: Boolean,
    strapTempStatus: Boolean,
    stapHVoltStatus: Boolean,
    strapLoop: Boolean,
    sfoLoop: LgsSfoControl,
    strapGate: Int,
    aoLoop: Boolean
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object EpicsAltairConfig

  /*
   * Helper function to be used until TcsEpics is converted to use F[_]
   */
  def convertTcsAction[T](a: SeqAction[T]): IO[T] = a.value.flatMap(_.fold(IO.raiseError(_), IO(_)))

  // This is a bit convoluted. AO follow state is read from Altair, but set as part of TCS configuration
  override def isFollowing: IO[Option[Boolean]] = AltairEpics.instance.aoFollow
}
