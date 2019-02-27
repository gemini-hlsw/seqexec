// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import java.time.LocalDate
import java.time.format.DateTimeFormatter

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

  private def newPosition(currCfg: EpicsAltairConfig)(prev: FocalPlaneOffset, next: FocalPlaneOffset)
  : (Length, Length) = {
    val dX = next.x - prev.x
    val dY = next.y - prev.y

    currCfg.guideStarCoords.bimap(_ + dX, _ + dY)
  }

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

  private def validControlMatrix(currMtxPos: (Length, Length))(newPos: (Length, Length)): Boolean = {
    val limit = Arcseconds(5.0) / FOCAL_PLANE_SCALE

    val diff = newPos.bimap(_ + currMtxPos._1, _ + currMtxPos._2)

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

  private def pauseNgsMode(fieldLens: FieldLens, currCfg: EpicsAltairConfig)(reasons: Set[Gaos.PauseCondition])
  : IO[Set[ResumeCondition] => IO[Unit]] = {
    val offsets = reasons.collectFirst { case OffsetMove(p, n) => (p, n) }
    val newPos = offsets.map(Function.tupled(newPosition(currCfg)))
    val newPosOk = newPos.forall(newPosInRange)
    val matrixOk = newPos.forall(validateCurrentControlMatrix(currCfg, _)) && fieldLens =!= FieldLens.IN
    val prepMatrixOk = newPos.forall(validatePreparedControlMatrix(currCfg, _)) && fieldLens =!= FieldLens.IN
    val guideOk = !reasons.contains(GaosStarOff) //It can follow the guide star on this step

    val needsToStop = !(newPosOk || matrixOk || guideOk)
    val newCfg = (EpicsAltairConfig.guideStarCoords.modify(v => newPos.filter(_ => newPosOk).getOrElse(v)) >>>
      EpicsAltairConfig.preparedMatrixCoords.modify(v => newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk)
        .getOrElse(v))) (currCfg)

    if (needsToStop) {
      convertTcsAction(epicsTcs.aoCorrect.setCorrections("OFF") *>
        epicsTcs.targetFilter.setShortCircuit("Closed")) *>
        newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk).fold(IO.unit)(prepareMatrix) *>
        convertTcsAction(epicsTcs.targetFilter.post) *>
        IO(resumeNgsMode(newCfg))
    }
    else IO(dumbResume)
  }

  private val dumbResume = (_: Set[ResumeCondition]) => IO.unit

  private def resumeNgsMode(currCfg: EpicsAltairConfig)(reasons: Set[Gaos.ResumeCondition]): IO[Unit] = {
    val newPosOk = newPosInRange(currCfg.guideStarCoords)
    val guideOk = reasons.contains(GaosStarOn)

    if (newPosOk && guideOk) {
      epicsAltair.waitMatrixCalc(CarStateGEM5.IDLE, 10.seconds) *>
        convertTcsAction(epicsTcs.aoCorrect.setCorrections("ON") *>
          epicsTcs.aoFlatten.mark *>
          epicsTcs.targetFilter.setShortCircuit("Open") *>
          epicsTcs.targetFilter.post.void
        )
    }
    else IO.unit
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

  private def startStrapGate(currCfg: EpicsAltairConfig): IO[Unit] = ( IO.unit
//    epicsAltair.strapGateControl.setGate(1) *>
//      epicsAltair.strapGateControl.post[IO] *>
//      epicsAltair.waitForStrapGate(100, 5.seconds)
    ).unlessA(currCfg.strapGate =!= 0)

  private def stopStrapGate(currCfg: EpicsAltairConfig): IO[Unit] = ( IO.unit
//    epicsAltair.strapGateControl.setGate(0) *>
//      epicsAltair.strapGateControl.post[IO].void
    ).unlessA(currCfg.strapGate === 0)

  private def startStrapLoop(currCfg: EpicsAltairConfig): IO[Unit] = ( IO.unit
//    epicsAltair.strapControl.setActive(1) *>
//      epicsAltair.strapControl.post[IO] *>
//      epicsAltair.waitForStrapLoop(v = true, 10.seconds)
    ).unlessA(currCfg.strapLoop)

  private def stopStrapLoop(currCfg: EpicsAltairConfig): IO[Unit] = ( IO.unit
//    epicsAltair.strapControl.setActive(0) *>
//      epicsAltair.strapControl.post[IO].void
    ).whenA(currCfg.strapLoop)

  implicit val sfoControlEq: Eq[LgsSfoControl] = Eq.by(_.ordinal)

  private def startSfoLoop(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.sfoControl.setActive(LgsSfoControl.Enable) *>
      epicsAltair.sfoControl.post[IO].void
    ).unlessA(currCfg.sfoLoop === LgsSfoControl.Enable)

  private def stopSfoLoop(currCfg: EpicsAltairConfig): IO[Unit] = (
    epicsAltair.sfoControl.setActive(LgsSfoControl.Pause) *>
      epicsAltair.sfoControl.post[IO].void
    ).unlessA(currCfg.sfoLoop === LgsSfoControl.Enable)

  private def ttgsOn(strap: Boolean, sfo: Boolean, currCfg: EpicsAltairConfig): IO[EpicsAltairConfig] =
    for {
      l1 <- strap.fold(
        checkStrapLoopState(currCfg).fold(IO.raiseError, IO(_)) *>
          startStrapGate(currCfg) *>
          startStrapLoop(currCfg) *>
          IO(EpicsAltairConfig.strapGate.set(100) >>> EpicsAltairConfig.strapLoop.set(true)),
        IO(identity[EpicsAltairConfig](_))
      )
      l2 <- sfo.fold(
        startSfoLoop(currCfg) *> IO(EpicsAltairConfig.sfoLoop.set(LgsSfoControl.Enable)),
        IO(identity[EpicsAltairConfig](_))
      )
    } yield (l1 >>> l2) (currCfg)

  private def ttgsOff(currCfg: EpicsAltairConfig): IO[EpicsAltairConfig] =
    stopStrapGate(currCfg) *>
      stopStrapLoop(currCfg) *>
      stopSfoLoop(currCfg) *>
      IO(
        (EpicsAltairConfig.strapGate.set(0) >>>
          EpicsAltairConfig.strapLoop.set(false) >>>
          EpicsAltairConfig.sfoLoop.modify { v =>
            (v === LgsSfoControl.Disable).fold(LgsSfoControl.Disable, LgsSfoControl.Pause)
          }) (currCfg)
      )

  private def pauseLgsMode(strap: Boolean, sfo: Boolean, fieldLens: FieldLens, currCfg: EpicsAltairConfig)(
    reasons: Set[Gaos.PauseCondition]): IO[Set[ResumeCondition] => IO[Unit]] = {

    val offsets = reasons.collectFirst { case OffsetMove(p, n) => (p, n) }
    val newPos = offsets.map(Function.tupled(newPosition(currCfg)))
    val newPosOk = newPos.forall(newPosInRange)
    val matrixOk = newPos.forall(validateCurrentControlMatrix(currCfg, _)) && fieldLens =!= FieldLens.IN
    val prepMatrixOk = newPos.forall(validatePreparedControlMatrix(currCfg, _)) && fieldLens =!= FieldLens.IN
    val guideOk = !reasons.contains(GaosStarOff) //It can follow the guide star on this step

    val needsToStop = !(newPosOk || matrixOk || guideOk)
    val newCfg = EpicsAltairConfig.guideStarCoords.modify(v => newPos.filter(_ => newPosOk).getOrElse(v)) >>>
      EpicsAltairConfig.preparedMatrixCoords.modify(v => newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk)
        .getOrElse(v))

    if (needsToStop) {
      for {
        c <- ttgsOff(currCfg)
        _ <- convertTcsAction(epicsTcs.aoCorrect.setCorrections("OFF"))
        _ <- convertTcsAction(epicsTcs.targetFilter.setShortCircuit("Closed"))
        _ <- newPos.filter(_ => newPosOk && !matrixOk && !prepMatrixOk).fold(IO.unit)(prepareMatrix)
        _ <- convertTcsAction(epicsTcs.targetFilter.post)
      } yield resumeLgsMode(strap, sfo, newCfg(c))(_)
    }
    else IO(dumbResume)
  }

  private def resumeLgsMode(strap: Boolean, sfo: Boolean, currCfg: EpicsAltairConfig)(
    reasons: Set[Gaos.ResumeCondition]): IO[Unit] = {
    val newPosOk = newPosInRange(currCfg.guideStarCoords)
    val guideOk = reasons.contains(GaosStarOn)

    if (newPosOk && guideOk) {
      epicsAltair.waitMatrixCalc(CarStateGEM5.IDLE, 10.seconds) *>
        convertTcsAction(epicsTcs.aoCorrect.setCorrections("ON") *>
          epicsTcs.aoFlatten.mark *>
          epicsTcs.targetFilter.setShortCircuit("Open") *>
          epicsTcs.targetFilter.post
        ) *>
        epicsAltair.btoLoopControl.setActive("ON") *>
        epicsAltair.btoLoopControl.post[IO] *>
        ttgsOn(strap, sfo, currCfg).void
    }
    else IO.unit

  }

  // TODO Should do someting if P1 is turned off ?
  private def pauseLgsWithP1Mode
  : IO[Set[ResumeCondition] => IO[Unit]] = IO(dumbResume)

  // TODO Should do someting if OI is turned off ?
  private def pauseLgsWithOiMode
  : IO[Set[ResumeCondition] => IO[Unit]] = IO(dumbResume)

  private def turnOff: IO[Set[ResumeCondition] => IO[Unit]] =
    convertTcsAction(epicsTcs.aoCorrect.setCorrections("OFF") *>
      epicsTcs.targetFilter.post
    ) *> IO(dumbResume)

  override def pause(reasons: Set[PauseCondition], fieldLens: FieldLens)(cfg: AltairConfig)
  : IO[Set[ResumeCondition] => IO[Unit]] =
    retrieveConfig.flatMap { currCfg =>
      cfg match {
        case Ngs(_) => pauseNgsMode(fieldLens, currCfg)(reasons)
        case Lgs(strap: Boolean, sfo: Boolean) => pauseLgsMode(strap, sfo, fieldLens, currCfg)(reasons)
        case LgsWithP1 => pauseLgsWithP1Mode
        case LgsWithOi => pauseLgsWithOiMode
        case AltairOff => turnOff
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
    aogsx <- getStatusVal(epicsTcs.aoGuideStarX.map(_.map(Millimeters(_))), "Altair guide star X", "TCS")
    aogsy <- getStatusVal(epicsTcs.aoGuideStarY.map(_.map(Millimeters(_))), "Altair guide star Y", "TCS")
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
  } yield EpicsAltairConfig(
    (aogsx, aogsy),
    (cmtxx, cmtxy),
    (pmtxx, pmtxy),
    strRT,
    strTm,
    strHV,
    strap,
    sfo,
    stGat
  )

  @Lenses
  final case class EpicsAltairConfig(
    guideStarCoords: (Length, Length),
    currentMatrixCoords: (Length, Length),
    preparedMatrixCoords: (Length, Length),
    strapRTStatus: Boolean,
    strapTempStatus: Boolean,
    stapHVoltStatus: Boolean,
    strapLoop: Boolean,
    sfoLoop: LgsSfoControl,
    strapGate: Int
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object EpicsAltairConfig

  /*
   * Helper function to be used until TcsEpics is converted to use F[_]
   */
  def convertTcsAction[T](a: SeqAction[T]): IO[T] = a.value.flatMap(_.fold(IO.raiseError(_), IO(_)))

}
