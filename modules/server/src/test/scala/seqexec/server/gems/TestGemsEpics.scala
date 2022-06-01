// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import mouse.boolean._
import edu.gemini.seqexec.server.gems.LoopState
import monocle.macros.Lenses
import seqexec.server.TestEpicsCommand.TestEpicsCommand2
import seqexec.server.gems.GemsControllerEpics.{
  InstrumentCondition,
  OffsetCondition,
  PauseCmd,
  ResumeCmd,
  UnguidedCondition
}
import seqexec.server.gems.GemsEpics.LoopControl

import scala.concurrent.duration.FiniteDuration

case class TestGemsEpics[F[_]: Sync](
  state: Ref[F, TestGemsEpics.State],
  out:   Ref[F, List[TestGemsEpics.LoopEvent]]
) extends GemsEpics[F] {
  import TestGemsEpics._

  val outputF: F[List[LoopEvent]] = out.get

  override val loopControl: GemsEpics.LoopControl[F] =
    new TestEpicsCommand2[F, State, LoopEvent, String, String](State.loopControlCmd, state, out)
      with LoopControl[F] {
      override protected def event(st: State): LoopEvent =
        LoopEvent(st.loopControlCmd.param1, st.loopControlCmd.param2)

      override protected def cmd(st: State): State = {
        val cmd          = st.loopControlCmd.param1
        val reasons      = st.loopControlCmd.param2
        val isDither     = reasons.contains(OffsetCondition)
        val isSky        = reasons.contains(UnguidedCondition)
        val isInstrument = reasons.contains(InstrumentCondition)

        if (cmd === PauseCmd) {
          val changeTTLoop    = State.ttLoop.modify(v =>
            ((isDither || isSky) && v == LoopState.CLOSED).fold(LoopState.PAUSED, v)
          )
          val changeAniLoop   = State.aniLoop.modify(v =>
            ((isSky || isDither) && v == LoopState.CLOSED).fold(LoopState.PAUSED, v)
          )
          val changeFlexLoop  = State.flexureLoop.modify(v =>
            ((isSky || isDither || isInstrument) && v == LoopState.CLOSED).fold(LoopState.PAUSED, v)
          )
          val changeFocusLoop = State.focusLoop.modify(v =>
            ((isSky || isDither) && v == LoopState.CLOSED).fold(LoopState.PAUSED, v)
          )
          val saveReasons     = State.pauseReasons
            .modify(x => isDither.fold(x + Dither, x))
            .andThen(State.pauseReasons.modify(x => isSky.fold(x + Sky, x)))
            .andThen(State.pauseReasons.modify(x => isInstrument.fold(x + Filter, x)))

          saveReasons
            .andThen(changeTTLoop)
            .andThen(changeAniLoop)
            .andThen(changeFlexLoop)
            .andThen(changeFocusLoop)(st)
        } else if (cmd === ResumeCmd) {
          val wasDither       = st.pauseReasons.contains(Dither)
          val wasSky          = st.pauseReasons.contains(Sky)
          val wasInstrument   = st.pauseReasons.contains(Filter)
          val changeTTLoop    = State.ttLoop.modify(v =>
            (((isDither && wasDither && (!wasSky || isSky)) || (wasSky && isSky)) && v == LoopState.PAUSED)
              .fold(LoopState.CLOSED, v)
          )
          val changeAniLoop   = State.aniLoop.modify(v =>
            (((isDither && wasDither && (!wasSky || isSky)) || (wasSky && isSky)) && v == LoopState.PAUSED)
              .fold(LoopState.CLOSED, v)
          )
          val changeFlexLoop  = State.flexureLoop.modify(v =>
            ((wasInstrument && isInstrument && (!wasSky || isSky) && (!wasDither || isDither)) && v == LoopState.PAUSED)
              .fold(LoopState.CLOSED, v)
          )
          val changeFocusLoop = State.focusLoop.modify(v =>
            (((isDither && wasDither && (!wasSky || isSky)) || (wasSky && isSky)) && v == LoopState.PAUSED)
              .fold(LoopState.CLOSED, v)
          )
          val saveReasons     = State.pauseReasons
            .modify(x => isDither.fold(x - Dither, x))
            .andThen(State.pauseReasons.modify(x => isSky.fold(x - Sky, x)))
            .andThen(State.pauseReasons.modify(x => isInstrument.fold(x - Filter, x)))

          saveReasons
            .andThen(changeTTLoop)
            .andThen(changeAniLoop)
            .andThen(changeFlexLoop)
            .andThen(changeFocusLoop)(st)
        } else st
      }

      override def setCommand(v: String): F[Unit] = setParameter1(v)

      override def setReasons(v: String): F[Unit] = setParameter2(v)
    }

  override def aniLoop: F[LoopState] = state.get.map(State.aniLoop.get)

  override def astrometryReady: F[Boolean] = state.get.map(State.astrometryReady.get)

  override def flexureLoop: F[LoopState] = state.get.map(State.flexureLoop.get)

  override def focusLoop: F[LoopState] = state.get.map(State.focusLoop.get)

  override def lgsFlux: F[List[Float]] = state.get.map(State.lgsFlux.get)

  override def lgsStrehl: F[Double] = state.get.map(State.lgsStrehl.get)

  override def rZero: F[Double] = state.get.map(State.rZero.get)

  override def cnSquare: F[List[Float]] = state.get.map(State.cnSquare.get)

  override def astroMode: F[String] = state.get.map(State.astroMode.get)

  override def lgsLoop: F[LoopState] = state.get.map(State.lgsLoop.get)

  override def lgsMatrixReady: F[Boolean] = state.get.map(State.lgsMatrixReady.get)

  override def cwfs1Used: F[Boolean] = state.get.map(State.cwfs1Used.get)

  override def cwfs2Used: F[Boolean] = state.get.map(State.cwfs2Used.get)

  override def cwfs3Used: F[Boolean] = state.get.map(State.cwfs3Used.get)

  override def cwfs1Magnitude: F[Double] = state.get.map(State.cwfs1Magnitude.get)

  override def cwfs2Magnitude: F[Double] = state.get.map(State.cwfs2Magnitude.get)

  override def cwfs3Magnitude: F[Double] = state.get.map(State.cwfs3Magnitude.get)

  override def ngsFlux: F[List[Int]] = state.get.map(State.ngsFlux.get)

  override def odgs1Used: F[Boolean] = state.get.map(State.odgs1Used.get)

  override def odgs2Used: F[Boolean] = state.get.map(State.odgs2Used.get)

  override def odgs3Used: F[Boolean] = state.get.map(State.odgs3Used.get)

  override def odgs4Used: F[Boolean] = state.get.map(State.odgs4Used.get)

  override def odgs1Magnitude: F[Double] = state.get.map(State.odgs1Magnitude.get)

  override def odgs2Magnitude: F[Double] = state.get.map(State.odgs2Magnitude.get)

  override def odgs3Magnitude: F[Double] = state.get.map(State.odgs3Magnitude.get)

  override def odgs4Magnitude: F[Double] = state.get.map(State.odgs4Magnitude.get)

  override def oigsUsed: F[Boolean] = state.get.map(State.oigsUsed.get)

  override def oigsMagnitude: F[Double] = state.get.map(State.oigsMagnitude.get)

  override def scienceReady: F[Boolean] = state.get.map(State.scienceReady.get)

  override def waitForStableLoops(timeout: FiniteDuration): F[Unit] = Applicative[F].unit

  override def ttLoop: F[LoopState] = state.get.map(State.ttLoop.get)

  override def lgsExpTime: F[Double] = state.get.map(State.lgsExpTime.get)

  override def ngsExpMult: F[Double] = state.get.map(State.ngsExpMult.get)

  override def sourceMask: F[Int] = state.get.map(State.sourceMask.get)

  override def apd1Active: F[Boolean] = state.get.map(State.apd1Active.get)

  override def apd2Active: F[Boolean] = state.get.map(State.apd2Active.get)

  override def apd3Active: F[Boolean] = state.get.map(State.apd3Active.get)

  override def scienceAdcLoopActive: F[Boolean] = state.get.map(State.scienceAdcLoopActive.get)

  override def ngsAdcLoopActive: F[Boolean] = state.get.map(State.ngsAdcLoopActive.get)

  override def scienceAdcState: F[String] = state.get.map(State.scienceAdcState.get)

  override def beamSplitterState: F[String] = state.get.map(State.beamSplitterState.get)
}

object TestGemsEpics {

  sealed trait PauseReason extends Product with Serializable
  case object Sky          extends PauseReason
  case object Dither       extends PauseReason
  case object Filter       extends PauseReason

  @Lenses
  case class State(
    aniLoop:              LoopState,
    astrometryReady:      Boolean,
    flexureLoop:          LoopState,
    focusLoop:            LoopState,
    lgsFlux:              List[Float],
    lgsStrehl:            Double,
    rZero:                Double,
    cnSquare:             List[Float],
    astroMode:            String,
    lgsLoop:              LoopState,
    lgsMatrixReady:       Boolean,
    cwfs1Used:            Boolean,
    cwfs2Used:            Boolean,
    cwfs3Used:            Boolean,
    cwfs1Magnitude:       Double,
    cwfs2Magnitude:       Double,
    cwfs3Magnitude:       Double,
    ngsFlux:              List[Int],
    odgs1Used:            Boolean,
    odgs2Used:            Boolean,
    odgs3Used:            Boolean,
    odgs4Used:            Boolean,
    odgs1Magnitude:       Double,
    odgs2Magnitude:       Double,
    odgs3Magnitude:       Double,
    odgs4Magnitude:       Double,
    oigsUsed:             Boolean,
    oigsMagnitude:        Double,
    scienceReady:         Boolean,
    ttLoop:               LoopState,
    lgsExpTime:           Double,
    ngsExpMult:           Double,
    sourceMask:           Int,
    apd1Active:           Boolean,
    apd2Active:           Boolean,
    apd3Active:           Boolean,
    scienceAdcLoopActive: Boolean,
    ngsAdcLoopActive:     Boolean,
    scienceAdcState:      String,
    beamSplitterState:    String,
    pauseReasons:         Set[PauseReason],
    loopControlCmd:       TestEpicsCommand2.State[String, String]
  )

  val defaultState: State = State(
    aniLoop = LoopState.OPEN,
    astrometryReady = false,
    flexureLoop = LoopState.OPEN,
    focusLoop = LoopState.OPEN,
    lgsFlux = List.fill(5)(0.0f),
    lgsStrehl = 0.0,
    rZero = 0.0,
    cnSquare = List.fill(32)(0.0f),
    astroMode = "None",
    lgsLoop = LoopState.OPEN,
    lgsMatrixReady = false,
    cwfs1Used = false,
    cwfs2Used = false,
    cwfs3Used = false,
    cwfs1Magnitude = 10,
    cwfs2Magnitude = 10,
    cwfs3Magnitude = 10,
    ngsFlux = List.fill(3)(0),
    odgs1Used = false,
    odgs2Used = false,
    odgs3Used = false,
    odgs4Used = false,
    odgs1Magnitude = 10,
    odgs2Magnitude = 10,
    odgs3Magnitude = 10,
    odgs4Magnitude = 10,
    oigsUsed = false,
    oigsMagnitude = 10,
    scienceReady = false,
    ttLoop = LoopState.OPEN,
    lgsExpTime = 0.0,
    ngsExpMult = 1,
    sourceMask = 0,
    apd1Active = false,
    apd2Active = false,
    apd3Active = false,
    scienceAdcLoopActive = false,
    ngsAdcLoopActive = false,
    scienceAdcState = "Out",
    beamSplitterState = "1",
    pauseReasons = Set.empty[PauseReason],
    loopControlCmd = TestEpicsCommand2.State(mark = false, "", "")
  )

  final case class LoopEvent(cmd: String, reasons: String)

  def build[F[_]: Sync](s0: TestGemsEpics.State): F[TestGemsEpics[F]] = for {
    st <- Ref.of(s0)
    ev <- Ref.of(List.empty[TestGemsEpics.LoopEvent])
  } yield TestGemsEpics(st, ev)

}
