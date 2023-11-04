// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.effect.Async
import cats.effect.Ref
import cats.implicits._
import edu.gemini.epics.acm.CarStateGEM5
import edu.gemini.seqexec.server.altair.LgsSfoControl
import monocle.macros.Lenses
import seqexec.server.TestEpicsCommand.TestEpicsCommand1
import seqexec.server.altair.AltairEpics.{
  BtoLoopControlCommand,
  SfoControlCommand,
  StrapControlCommand,
  StrapGateControlCommand
}

import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.FiniteDuration

case class TestAltairEpics[F[_]: Async](
  state: Ref[F, TestAltairEpics.State],
  out:   Ref[F, List[TestAltairEpics.Event]]
) extends AltairEpics[F] {
  import TestAltairEpics._

  val outputF: F[List[Event]] = out.get

  override val strapGateControl: StrapGateControlCommand[F] =
    new TestEpicsCommand1[F, State, Event, Int](State.strapGateControlCmd, state, out)
      with StrapGateControlCommand[F] {
      override def setGate(v: Int): F[Unit] = setParameter1(v)

      override protected def event(st: State): Event =
        Event.StrapGateControlCmd(st.strapGateControlCmd.param1)

      override protected def cmd(st: State): State =
        st.copy(strapGate = st.strapGateControlCmd.param1)
    }
  override val strapControl: StrapControlCommand[F]         =
    new TestEpicsCommand1[F, State, Event, Int](State.strapControlCmd, state, out)
      with StrapControlCommand[F] {
      override def setActive(v: Int): F[Unit] = setParameter1(v)

      override protected def event(st: State): Event =
        Event.StrapControlCmd(st.strapControlCmd.param1)

      override protected def cmd(st: State): State = st.copy(strapGate = st.strapControlCmd.param1)
    }
  override val sfoControl: SfoControlCommand[F]             =
    new TestEpicsCommand1[F, State, Event, LgsSfoControl](State.sfoControlCmd, state, out)
      with SfoControlCommand[F] {
      override def setActive(v: LgsSfoControl): F[Unit] = setParameter1(v)

      override protected def event(st: State): Event = Event.SfoControlCmd(st.sfoControlCmd.param1)

      override protected def cmd(st: State): State = st.copy(sfoLoop = st.sfoControlCmd.param1)
    }
  override val btoLoopControl: BtoLoopControlCommand[F]     =
    new TestEpicsCommand1[F, State, Event, String](State.btoControlCmd, state, out)
      with BtoLoopControlCommand[F] {
      override def setActive(v: String): F[Unit] = setParameter1(v)

      override protected def event(st: State): Event = Event.BtoControlCmd(st.btoControlCmd.param1)

      override protected def cmd(st: State): State = st.copy(btoLoop = st.btoControlCmd.param1)
    }

  override def waitForStrapGate(v: Int, timeout: FiniteDuration): F[Unit] = Applicative[F].unit

  override def waitForStrapLoop(v: Boolean, timeout: FiniteDuration): F[Unit] = Applicative[F].unit

  override def waitAoSettled(timeout: FiniteDuration): F[Unit] = Applicative[F].unit

  override def waitMatrixCalc(v: CarStateGEM5, timeout: FiniteDuration): F[Unit] =
    Applicative[F].unit

  override def strapTempStatus: F[Boolean] = state.get.map(_.strapTempStatus)

  override def strapGate: F[Int] = state.get.map(_.strapGate)

  override def strapLoop: F[Boolean] = state.get.map(_.strapLoop)

  override def strapRTStatus: F[Boolean] = state.get.map(_.strapRTStatus)

  override def strapHVStatus: F[Boolean] = state.get.map(_.strapHVStatus)

  override def sfoLoop: F[LgsSfoControl] = state.get.map(_.sfoLoop)

  override def aoexpt: F[Float] = state.get.map(_.aoexpt)

  override def aocounts: F[Double] = state.get.map(_.aocounts)

  override def aoseeing: F[Float] = state.get.map(_.aoseeing)

  override def aowfsx: F[Double] = state.get.map(_.aowfsx)

  override def aowfsy: F[Double] = state.get.map(_.aowfsy)

  override def aowfsz: F[Double] = state.get.map(_.aowfsz)

  override def aogain: F[Double] = state.get.map(_.aogain)

  override def aoncpa: F[String] = state.get.map(_.aoncpa)

  override def ngndfilt: F[String] = state.get.map(_.ngndfilt)

  override def astar: F[String] = state.get.map(_.astar)

  override def aoflex: F[String] = state.get.map(_.aoflex)

  override def lgustage: F[String] = state.get.map(_.lgustage)

  override def aobs: F[String] = state.get.map(_.aobs)

  override def aoLoop: F[Boolean] = state.get.map(_.aoLoop)

  override def aoSettled: F[Boolean] = state.get.map(_.aoSettled)

  override def matrixStartX: F[Double] = state.get.map(_.matrixStartX)

  override def matrixStartY: F[Double] = state.get.map(_.matrixStartY)

  override def controlMatrixCalc: F[CarStateGEM5] = state.get.map(_.controlMatrixCalc)

  override def lgsP1: F[Boolean] = state.get.map(_.lgsP1)

  override def lgsOi: F[Boolean] = state.get.map(_.lgsOi)

  override def aoFollow: F[Boolean] = state.get.map(_.aoFollow)

  override def lgdfocus: F[Double] = state.get.map(_.lgdfocus)

  override def apd1: F[Float] = state.get.map(_.apd1)

  override def apd2: F[Float] = state.get.map(_.apd2)

  override def apd3: F[Float] = state.get.map(_.apd3)

  override def apd4: F[Float] = state.get.map(_.apd4)

  override def lgttexp: F[Int] = state.get.map(_.lgttexp)

  override def fsmtip: F[Double] = state.get.map(_.fsmtip)

  override def fsmtilt: F[Double] = state.get.map(_.fsmtilt)

  override def lgzmpos: F[Double] = state.get.map(_.lgzmpos)

  override def aozoom: F[Double] = state.get.map(_.aozoom)

  override def aoza: F[Double] = state.get.map(_.aoza)

  override def nathick: F[Double] = state.get.map(_.nathick)

  override def lgndfilt: F[String] = state.get.map(_.lgndfilt)

  override def lgttiris: F[String] = state.get.map(_.lgttiris)

  override def lgsfcnts: F[Double] = state.get.map(_.lgsfcnts)

  override def lgsfexp: F[Double] = state.get.map(_.lgsfexp)

}

object TestAltairEpics {
  val DefaultTimeout: FiniteDuration = FiniteDuration(1, SECONDS)

  @Lenses
  final case class State(
    strapTempStatus:     Boolean,
    strapGate:           Int,
    strapLoop:           Boolean,
    strapRTStatus:       Boolean,
    strapHVStatus:       Boolean,
    sfoLoop:             LgsSfoControl,
    aoexpt:              Float,
    aocounts:            Double,
    aoseeing:            Float,
    aowfsx:              Double,
    aowfsy:              Double,
    aowfsz:              Double,
    aogain:              Double,
    aoncpa:              String,
    ngndfilt:            String,
    astar:               String,
    aoflex:              String,
    lgustage:            String,
    aobs:                String,
    aoLoop:              Boolean,
    aoSettled:           Boolean,
    matrixStartX:        Double,
    matrixStartY:        Double,
    controlMatrixCalc:   CarStateGEM5,
    lgsP1:               Boolean,
    lgsOi:               Boolean,
    aoFollow:            Boolean,
    lgdfocus:            Double,
    apd1:                Float,
    apd2:                Float,
    apd3:                Float,
    apd4:                Float,
    lgttexp:             Int,
    fsmtip:              Double,
    fsmtilt:             Double,
    lgzmpos:             Double,
    aozoom:              Double,
    aoza:                Double,
    nathick:             Double,
    lgndfilt:            String,
    lgttiris:            String,
    lgsfcnts:            Double,
    lgsfexp:             Double,
    btoLoop:             String,
    strapGateControlCmd: TestEpicsCommand1.State[Int],
    strapControlCmd:     TestEpicsCommand1.State[Int],
    sfoControlCmd:       TestEpicsCommand1.State[LgsSfoControl],
    btoControlCmd:       TestEpicsCommand1.State[String]
  )

  sealed trait Event
  object Event {
    final case class StrapGateControlCmd(newState: Int)     extends Event
    final case class StrapControlCmd(newState: Int)         extends Event
    final case class SfoControlCmd(newState: LgsSfoControl) extends Event
    final case class BtoControlCmd(newState: String)        extends Event
  }

  val defaultState: State = State(
    strapTempStatus = false,
    strapGate = 0,
    strapLoop = false,
    strapRTStatus = false,
    strapHVStatus = false,
    sfoLoop = LgsSfoControl.Disable,
    aoexpt = 0.0f,
    aocounts = 0,
    aoseeing = 0.0f,
    aowfsx = 0.0,
    aowfsy = 0.0,
    aowfsz = 0.0,
    aogain = 0.0,
    aoncpa = "",
    ngndfilt = "",
    astar = "",
    aoflex = "",
    lgustage = "",
    aobs = "",
    aoLoop = false,
    aoSettled = false,
    matrixStartX = 0.0,
    matrixStartY = 0.0,
    controlMatrixCalc = CarStateGEM5.IDLE,
    lgsP1 = false,
    lgsOi = false,
    aoFollow = false,
    lgdfocus = 0.0,
    apd1 = 0.0f,
    apd2 = 0.0f,
    apd3 = 0.0f,
    apd4 = 0.0f,
    lgttexp = 0,
    fsmtip = 0.0,
    fsmtilt = 0.0,
    lgzmpos = 0.0,
    aozoom = 0.0,
    aoza = 0.0,
    nathick = 0.0,
    lgndfilt = "",
    lgttiris = "",
    lgsfcnts = 0,
    lgsfexp = 0.0,
    btoLoop = "OFF",
    strapGateControlCmd = TestEpicsCommand1.State[Int](false, 0),
    strapControlCmd = TestEpicsCommand1.State[Int](false, 0),
    sfoControlCmd = TestEpicsCommand1.State[LgsSfoControl](false, LgsSfoControl.Disable),
    btoControlCmd = TestEpicsCommand1.State[String](false, "OFF")
  )

  def build[F[_]: Async](baseState: TestAltairEpics.State): F[TestAltairEpics[F]] =
    for {
      st  <- Ref.of(baseState)
      out <- Ref.of(List.empty[TestAltairEpics.Event])
    } yield TestAltairEpics[F](st, out)

}
