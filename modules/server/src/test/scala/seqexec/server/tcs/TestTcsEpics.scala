// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.{ Applicative, Eq }
import cats.effect.Async
import cats.syntax.all._
import mouse.boolean._
import edu.gemini.seqexec.server.tcs.{ BinaryOnOff, BinaryYesNo }
import monocle.{ Getter, Lens }
import monocle.macros.Lenses
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.TestEpicsCommand._
import seqexec.server.EpicsCommand
import seqexec.server.tcs.TcsEpics._
import seqexec.server.tcs.TestTcsEpics.TestTcsEvent.{ AoCorrectCmd, AoPrepareMatrix }
import squants.Angle
import squants.space.AngleConversions._

import java.util.concurrent.TimeUnit.SECONDS
import java.time.Duration
import scala.concurrent.duration.FiniteDuration
import cats.effect.{ Ref, Temporal }
import squants.space.Length

case class TestTcsEpics[F[_]: Async](
  state: Ref[F, TestTcsEpics.State],
  out:   Ref[F, List[TestTcsEpics.TestTcsEvent]]
) extends TcsEpics[F] {
  import TestTcsEpics._

  val outputF: F[List[TestTcsEvent]] = out.get

  val DefaultTimeout: FiniteDuration = FiniteDuration(1, SECONDS)

  override def post(timeout: FiniteDuration): F[ApplyCommandResult] =
    List[EpicsCommand[F]](
      m1GuideCmd,
      m2GuideCmd,
      m2GuideModeCmd,
      m2GuideModeCmd,
      m2GuideConfigCmd,
      mountGuideCmd,
      offsetACmd,
      offsetBCmd,
      wavelSourceA,
      pwfs1Park,
      pwfs1ProbeFollowCmd,
      pwfs1ProbeGuideCmd,
      pwfs1ObserveCmd,
      pwfs1StopObserveCmd,
      pwfs2Park,
      pwfs2ProbeFollowCmd,
      pwfs2ProbeGuideCmd,
      pwfs2ObserveCmd,
      pwfs2StopObserveCmd,
      oiwfsPark,
      oiwfsProbeFollowCmd,
      oiwfsProbeGuideCmd,
      oiwfsObserveCmd,
      oiwfsStopObserveCmd,
      offsetACmd,
      cwfs1ParkCmd,
      cwfs2ParkCmd,
      cwfs3ParkCmd,
      odgw1ParkCmd,
      odgw2ParkCmd,
      odgw3ParkCmd,
      odgw4ParkCmd,
      cwfs1FollowCmd,
      cwfs2FollowCmd,
      cwfs3FollowCmd,
      odgw1FollowCmd,
      odgw2FollowCmd,
      odgw3FollowCmd,
      odgw4FollowCmd
    ).map(_.post(DefaultTimeout))
      .sequence
      .map(_.find(_ =!= ApplyCommandResult.Completed).getOrElse(ApplyCommandResult.Completed))

  override val m1GuideCmd: M1GuideCmd[F]             =
    new TestEpicsCommand1[F, State, TestTcsEvent, String](State.m1GuideCmd, state, out)
      with M1GuideCmd[F] {
      override def setState(v: String): F[Unit] = setParameter1(v)

      override protected def event(st: State): TestTcsEvent =
        TestTcsEvent.M1GuideCmd(st.m1GuideCmd.param1)

      override protected def cmd(st: State): State =
        st.copy(m1Guide = if (st.m1GuideCmd.param1 === "on") BinaryOnOff.On else BinaryOnOff.Off)
    }
  override val m2GuideCmd: M2GuideCmd[F]             =
    new TestEpicsCommand1[F, State, TestTcsEvent, String](State.m2GuideCmd, state, out)
      with M2GuideCmd[F] {
      override def setState(v: String): F[Unit] = setParameter1(v)

      override protected def event(st: State): TestTcsEvent =
        TestTcsEvent.M2GuideCmd(st.m2GuideCmd.param1)

      override protected def cmd(st: State): State =
        st.copy(m2GuideState =
          if (st.m2GuideCmd.param1 === "on") BinaryOnOff.On else BinaryOnOff.Off
        )
    }
  override val m2GuideModeCmd: M2GuideModeCmd[F]     =
    new TestEpicsCommand1[F, State, TestTcsEvent, String](State.m2GuideModeCmd, state, out)
      with M2GuideModeCmd[F] {
      override def setComa(v: String): F[Unit] = setParameter1(v)

      override protected def event(st: State): TestTcsEvent =
        TestTcsEvent.M2GuideModeCmd(st.m2GuideModeCmd.param1)

      override protected def cmd(st: State): State =
        st.copy(comaCorrect = if (st.m2GuideModeCmd.param1 === "on") "On" else "Off")
    }
  override val m2GuideConfigCmd: M2GuideConfigCmd[F] =
    new TestEpicsCommand3[F, State, TestTcsEvent, String, String, String](State.m2GuideConfigCmd,
                                                                          state,
                                                                          out
    ) with M2GuideConfigCmd[F] {
      override def setSource(v: String): F[Unit] = setParameter1(v)

      override def setBeam(v: String): F[Unit] = setParameter2(v)

      override def setReset(v: String): F[Unit] = setParameter3(v)

      override protected def event(st: State): TestTcsEvent = TestTcsEvent.M2GuideConfigCmd(
        st.m2GuideConfigCmd.param1,
        st.m2GuideConfigCmd.param2,
        st.m2GuideConfigCmd.param3
      )

      override protected def cmd(st: State): State = st
    }
  override val mountGuideCmd: MountGuideCmd[F]       =
    new TestEpicsCommand2[F, State, TestTcsEvent, String, String](State.mountGuideCmd, state, out)
      with MountGuideCmd[F] {
      override def setSource(v: String): F[Unit] = setParameter1(v)

      override def setP1Weight(v: Double): F[Unit] = Applicative[F].unit

      override def setP2Weight(v: Double): F[Unit] = Applicative[F].unit

      override def setMode(v: String): F[Unit] = setParameter2(v)

      override protected def event(st: State): TestTcsEvent = TestTcsEvent.MountGuideCmd(
        st.mountGuideCmd.param1,
        st.mountGuideCmd.param2
      )

      override protected def cmd(st: State): State =
        st.copy(absorbTipTilt = if (st.mountGuideCmd.param2 === "on") 1 else 0)
    }
  override val offsetACmd: OffsetCmd[F]              =
    new TestEpicsCommand2[F, State, TestTcsEvent, Double, Double](State.offsetACmd, state, out)
      with OffsetCmd[F] {
      override def setX(v: Double): F[Unit] = setParameter1(v)

      override def setY(v: Double): F[Unit] = setParameter2(v)

      override protected def event(st: State): TestTcsEvent = TestTcsEvent.OffsetACmd(
        st.offsetACmd.param1,
        st.offsetACmd.param2
      )

      override protected def cmd(st: State): State =
        st.copy(xoffsetPoA1 = st.offsetACmd.param1, yoffsetPoA1 = st.offsetACmd.param2)
    }

  override val offsetBCmd: OffsetCmd[F] = new DummyCmd[F] with OffsetCmd[F] {
    override def setX(v: Double): F[Unit] = Applicative[F].unit
    override def setY(v: Double): F[Unit] = Applicative[F].unit
  }

  override val wavelSourceA: TargetWavelengthCmd[F] =
    new TestEpicsCommand1[F, State, TestTcsEvent, Double](State.wavelSourceACmd, state, out)
      with TargetWavelengthCmd[F] {
      override def setWavel(v: Double): F[Unit] = setParameter1(v)

      override protected def event(st: State): TestTcsEvent =
        TestTcsEvent.WavelSourceACmd(st.wavelSourceACmd.param1)

      override protected def cmd(st: State): State =
        st.copy(sourceAWavelength = st.wavelSourceACmd.param1)
    }

  override val wavelSourceB: TargetWavelengthCmd[F] = new DummyCmd[F] with TargetWavelengthCmd[F] {
    override def setWavel(v: Double): F[Unit] = Applicative[F].unit
  }

  override val m2Beam: M2Beam[F] = new DummyCmd[F] with M2Beam[F] {
    override def setBeam(v: String): F[Unit] = Applicative[F].unit
  }

  override val pwfs1ProbeGuideCmd: ProbeGuideCmd[F] =
    probeGuideConfigCmd(State.pwfs1ProbeGuideConfigCmd,
                        State.pwfs1ProbeGuideConfig,
                        TestTcsEvent.Pwfs1ProbeGuideConfig.apply
    )

  override val pwfs2ProbeGuideCmd: ProbeGuideCmd[F] =
    probeGuideConfigCmd(State.pwfs2ProbeGuideConfigCmd,
                        State.pwfs2ProbeGuideConfig,
                        TestTcsEvent.Pwfs2ProbeGuideConfig.apply
    )

  override val oiwfsProbeGuideCmd: ProbeGuideCmd[F] =
    probeGuideConfigCmd(State.oiwfsProbeGuideConfigCmd,
                        State.oiwfsProbeGuideConfig,
                        TestTcsEvent.OiwfsProbeGuideConfig.apply
    )

  override val pwfs1ProbeFollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.pwfs1ProbeFollowCmd,
    State.p1FollowS,
    State.p1Parked,
    TestTcsEvent.Pwfs1ProbeFollowCmd
  )

  override val pwfs2ProbeFollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.pwfs2ProbeFollowCmd,
    State.p2FollowS,
    State.p2Parked,
    TestTcsEvent.Pwfs2ProbeFollowCmd
  )

  override val oiwfsProbeFollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.oiwfsProbeFollowCmd,
    State.oiFollowS,
    State.oiParked,
    TestTcsEvent.OiwfsProbeFollowCmd
  )

  override val aoProbeFollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.aoProbeFollowCmd,
    State.aoFollowS,
    State.aoParked,
    TestTcsEvent.AoProbeFollowCmd
  )

  override val pwfs1Park: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.pwfs1ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Pwfs1ParkCmd

      override protected def cmd(st: State): State = st.copy(p1FollowS = "Off", p1Parked = true)
    }

  override val pwfs2Park: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.pwfs2ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Pwfs2ParkCmd

      override protected def cmd(st: State): State = st.copy(p2FollowS = "Off", p2Parked = true)
    }

  override val oiwfsPark: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.oiwfsParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.OiwfsParkCmd

      override protected def cmd(st: State): State = st.copy(oiFollowS = "Off", oiParked = true)
    }

  override val pwfs1StopObserveCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.pwfs1StopObserveCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Pwfs1StopObserveCmd

      override protected def cmd(st: State): State = st.copy(pwfs1On = BinaryYesNo.No)
    }

  override val pwfs2StopObserveCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.pwfs2StopObserveCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Pwfs2StopObserveCmd

      override protected def cmd(st: State): State = st.copy(pwfs2On = BinaryYesNo.No)
    }

  override val oiwfsStopObserveCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.oiwfsStopObserveCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.OiwfsStopObserveCmd

      override protected def cmd(st: State): State = st.copy(oiwfsOn = BinaryYesNo.No)
    }

  override val pwfs1ObserveCmd: WfsObserveCmd[F] =
    wfsObserveCmd(State.pwfs1ObserveCmd, State.pwfs1On, TestTcsEvent.Pwfs1ObserveCmd)

  override val pwfs2ObserveCmd: WfsObserveCmd[F] =
    wfsObserveCmd(State.pwfs2ObserveCmd, State.pwfs2On, TestTcsEvent.Pwfs2ObserveCmd)

  override val oiwfsObserveCmd: WfsObserveCmd[F] =
    wfsObserveCmd(State.oiwfsObserveCmd, State.oiwfsOn, TestTcsEvent.OiwfsObserveCmd)

  override val hrwfsParkCmd: EpicsCommand[F] = new DummyCmd[F]

  override val hrwfsPosCmd: HrwfsPosCmd[F] = new DummyCmd[F] with HrwfsPosCmd[F] {
    override def setHrwfsPos(v: String): F[Unit] = Applicative[F].unit
  }

  override val scienceFoldParkCmd: EpicsCommand[F] = new DummyCmd[F]

  override val scienceFoldPosCmd: ScienceFoldPosCmd[F] = new DummyCmd[F] with ScienceFoldPosCmd[F] {
    override def setScfold(v: String): F[Unit] = Applicative[F].unit
  }

  override val observe: EpicsCommand[F] = new DummyCmd[F]

  override val endObserve: EpicsCommand[F] = new DummyCmd[F]

  override val instrumentDefocusCmd: InstrumentDefocusCmd[F] =
    new DummyCmd[F] with InstrumentDefocusCmd[F] {
      def setDefocus(v: Length): F[Unit] = Applicative[F].unit
    }

  override val aoCorrect: AoCorrect[F] =
    new TestEpicsCommand2[F, State, TestTcsEvent, String, Int](State.aoCorrectCmd, state, out)
      with AoCorrect[F] {
      override def setCorrections(v: String): F[Unit] = setParameter1(v)

      override def setGains(v: Int): F[Unit] = setParameter2(v)

      override def setMatrix(v: Int): F[Unit] = Applicative[F].unit

      override protected def event(st: State): TestTcsEvent = AoCorrectCmd(
        st.aoCorrectCmd.param1,
        st.aoCorrectCmd.param2
      )

      override protected def cmd(st: State): State = st.copy(
        aoCorrect = st.aoCorrectCmd.param1,
        aoGains = st.aoCorrectCmd.param2
      )
    }

  override val aoPrepareControlMatrix: AoPrepareControlMatrix[F] =
    new TestEpicsCommand2[F, State, TestTcsEvent, Double, Double](State.aoPrepareControlMatrixCmd,
                                                                  state,
                                                                  out
    ) with AoPrepareControlMatrix[F] {
      override def setX(v: Double): F[Unit] = setParameter1(v)

      override def setY(v: Double): F[Unit] = setParameter2(v)

      override def setSeeing(v: Double): F[Unit] = Applicative[F].unit

      override def setStarMagnitude(v: Double): F[Unit] = Applicative[F].unit

      override def setWindSpeed(v: Double): F[Unit] = Applicative[F].unit

      override protected def event(st: State): TestTcsEvent = AoPrepareMatrix(
        st.aoPrepareControlMatrixCmd.param1,
        st.aoPrepareControlMatrixCmd.param2
      )

      override protected def cmd(st: State): State = st.copy(
        aoPreparedCMX = st.aoPrepareControlMatrixCmd.param1,
        aoPreparedCMY = st.aoPrepareControlMatrixCmd.param2
      )
    }

  override val aoFlatten: EpicsCommand[F] = new DummyCmd[F]

  override val aoStatistics: AoStatistics[F] = new DummyCmd[F] with AoStatistics[F] {
    override def setFileName(v: String): F[Unit] = Applicative[F].unit

    override def setSamples(v: Int): F[Unit] = Applicative[F].unit

    override def setInterval(v: Double): F[Unit] = Applicative[F].unit

    override def setTriggerTimeInterval(v: Double): F[Unit] = Applicative[F].unit
  }

  override val targetFilter: TargetFilter[F] = new DummyCmd[F] with TargetFilter[F] {
    override def setBandwidth(v: Double): F[Unit] = Applicative[F].unit

    override def setMaxVelocity(v: Double): F[Unit] = Applicative[F].unit

    override def setGrabRadius(v: Double): F[Unit] = Applicative[F].unit

    override def setShortCircuit(v: String): F[Unit] = Applicative[F].unit
  }

  override def absorbTipTilt: F[Int] = state.get.map(_.absorbTipTilt)

  override def m1GuideSource: F[String] = state.get.map(_.m1GuideSource)

  override def m1Guide: F[BinaryOnOff] = state.get.map(_.m1Guide)

  override def m2p1Guide: F[String] = state.get.map(_.m2p1Guide)

  override def m2p2Guide: F[String] = state.get.map(_.m2p2Guide)

  override def m2oiGuide: F[String] = state.get.map(_.m2oiGuide)

  override def m2aoGuide: F[String] = state.get.map(_.m2aoGuide)

  override def comaCorrect: F[String] = state.get.map(_.comaCorrect)

  override def m2GuideState: F[BinaryOnOff] = state.get.map(_.m2GuideState)

  override def xoffsetPoA1: F[Double] = state.get.map(_.xoffsetPoA1)

  override def yoffsetPoA1: F[Double] = state.get.map(_.yoffsetPoA1)

  override def xoffsetPoB1: F[Double] = state.get.map(_.xoffsetPoB1)

  override def yoffsetPoB1: F[Double] = state.get.map(_.yoffsetPoB1)

  override def xoffsetPoC1: F[Double] = state.get.map(_.xoffsetPoC1)

  override def yoffsetPoC1: F[Double] = state.get.map(_.yoffsetPoC1)

  override def sourceAWavelength: F[Double] = state.get.map(_.sourceAWavelength)

  override def sourceBWavelength: F[Double] = state.get.map(_.sourceBWavelength)

  override def sourceCWavelength: F[Double] = state.get.map(_.sourceCWavelength)

  override def chopBeam: F[String] = state.get.map(_.chopBeam)

  override def p1FollowS: F[String] = state.get.map(_.p1FollowS)

  override def p2FollowS: F[String] = state.get.map(_.p2FollowS)

  override def oiFollowS: F[String] = state.get.map(_.oiFollowS)

  override def aoFollowS: F[String] = state.get.map(_.aoFollowS)

  override def p1Parked: F[Boolean] = state.get.map(_.p1Parked)

  override def p2Parked: F[Boolean] = state.get.map(_.p2Parked)

  override def oiName: F[String] = state.get.map(_.oiName)

  override def oiParked: F[Boolean] = state.get.map(_.oiParked)

  override def pwfs1On: F[BinaryYesNo] = state.get.map(_.pwfs1On)

  override def pwfs2On: F[BinaryYesNo] = state.get.map(_.pwfs2On)

  override def oiwfsOn: F[BinaryYesNo] = state.get.map(_.oiwfsOn)

  override def sfName: F[String] = state.get.map(_.sfName)

  override def sfParked: F[Int] = state.get.map(_.sfParked)

  override def agHwName: F[String] = state.get.map(_.agHwName)

  override def agHwParked: F[Int] = state.get.map(_.agHwParked)

  override def instrAA: F[Double] = state.get.map(_.instrAA)

  override def inPosition: F[String] = state.get.map(_.inPosition)

  override def defocusA: F[Double] = state.get.map(_.defocusA)

  override def defocusB: F[Double] = state.get.map(_.defocusB)

  override def agInPosition: F[Double] = state.get.map(_.agInPosition)

  override val pwfs1ProbeGuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.pwfs1ProbeGuideConfig.asGetter)

  override val pwfs2ProbeGuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.pwfs2ProbeGuideConfig.asGetter)

  override val oiwfsProbeGuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.oiwfsProbeGuideConfig.asGetter)

  override def waitInPosition(stabilizationTime: Duration, timeout: FiniteDuration)(implicit
    T: Temporal[F]
  ): F[Unit] =
    Applicative[F].unit

  override def waitAGInPosition(timeout: FiniteDuration)(implicit T: Temporal[F]): F[Unit] =
    Applicative[F].unit

  override def hourAngle: F[String] = state.get.map(_.hourAngle)

  override def localTime: F[String] = state.get.map(_.localTime)

  override def trackingFrame: F[String] = state.get.map(_.trackingFrame)

  override def trackingEpoch: F[Double] = state.get.map(_.trackingEpoch)

  override def equinox: F[Double] = state.get.map(_.equinox)

  override def trackingEquinox: F[String] = state.get.map(_.trackingEquinox)

  override def trackingDec: F[Double] = state.get.map(_.trackingDec)

  override def trackingRA: F[Double] = state.get.map(_.trackingRA)

  override def elevation: F[Double] = state.get.map(_.elevation)

  override def azimuth: F[Double] = state.get.map(_.azimuth)

  override def crPositionAngle: F[Double] = state.get.map(_.crPositionAngle)

  override def ut: F[String] = state.get.map(_.ut)

  override def date: F[String] = state.get.map(_.date)

  override def m2Baffle: F[String] = state.get.map(_.m2Baffle)

  override def m2CentralBaffle: F[String] = state.get.map(_.m2CentralBaffle)

  override def st: F[String] = state.get.map(_.st)

  override def sfRotation: F[Double] = state.get.map(_.sfRotation)

  override def sfTilt: F[Double] = state.get.map(_.sfTilt)

  override def sfLinear: F[Double] = state.get.map(_.sfLinear)

  override def instrPA: F[Double] = state.get.map(_.instrPA)

  override def targetA: F[List[Double]] = state.get.map(_.targetA)

  override def aoFoldPosition: F[String] = state.get.map(_.aoFoldPosition)

  override def useAo: F[BinaryYesNo] = state.get.map(_.useAo)

  override def airmass: F[Double] = state.get.map(_.airmass)

  override def airmassStart: F[Double] = state.get.map(_.airmassStart)

  override def airmassEnd: F[Double] = state.get.map(_.airmassEnd)

  override def carouselMode: F[String] = state.get.map(_.carouselMode)

  override def crFollow: F[Int] = state.get.map(_.crFollow)

  override def crTrackingFrame: F[String] = state.get.map(_.crTrackingFrame)

  override def sourceATarget: Target[F] = targetGetters(state, State.sourceATarget.asGetter)

  override val pwfs1Target: Target[F] = targetGetters(state, State.pwfs1Target.asGetter)
  override val pwfs2Target: Target[F] = targetGetters(state, State.pwfs2Target.asGetter)
  override val oiwfsTarget: Target[F] = targetGetters(state, State.oiwfsTarget.asGetter)

  override def parallacticAngle: F[Angle] = state.get.map(_.parallacticAngle)

  override def m2UserFocusOffset: F[Double] = state.get.map(_.m2UserFocusOffset)

  override def pwfs1IntegrationTime: F[Double] = state.get.map(_.pwfs1IntegrationTime)

  override def pwfs2IntegrationTime: F[Double] = state.get.map(_.pwfs2IntegrationTime)

  override def oiwfsIntegrationTime: F[Double] = state.get.map(_.oiwfsIntegrationTime)

  override def gsaoiPort: F[Int] = state.get.map(_.gsaoiPort)

  override def gpiPort: F[Int] = state.get.map(_.gpiPort)

  override def f2Port: F[Int] = state.get.map(_.f2Port)

  override def niriPort: F[Int] = state.get.map(_.niriPort)

  override def gnirsPort: F[Int] = state.get.map(_.gnirsPort)

  override def nifsPort: F[Int] = state.get.map(_.nifsPort)

  override def gmosPort: F[Int] = state.get.map(_.gmosPort)

  override def ghostPort: F[Int] = state.get.map(_.ghostPort)

  override def igrins2Port: F[Int] = state.get.map(_.igrins2Port)

  override def aoGuideStarX: F[Double] = state.get.map(_.aoGuideStarX)

  override def aoGuideStarY: F[Double] = state.get.map(_.aoGuideStarY)

  override def aoPreparedCMX: F[Double] = state.get.map(_.aoPreparedCMX)

  override def aoPreparedCMY: F[Double] = state.get.map(_.aoPreparedCMY)

  override val g1ProbeGuideCmd: ProbeGuideCmd[F] = probeGuideConfigCmd(
    State.g1ProbeGuideConfigCmd,
    State.g1GuideConfig,
    TestTcsEvent.G1ProbeGuideConfig.apply
  )

  override val g2ProbeGuideCmd: ProbeGuideCmd[F] = probeGuideConfigCmd(
    State.g2ProbeGuideConfigCmd,
    State.g2GuideConfig,
    TestTcsEvent.G2ProbeGuideConfig.apply
  )

  override val g3ProbeGuideCmd: ProbeGuideCmd[F] = probeGuideConfigCmd(
    State.g3ProbeGuideConfigCmd,
    State.g3GuideConfig,
    TestTcsEvent.G3ProbeGuideConfig.apply
  )

  override val g4ProbeGuideCmd: ProbeGuideCmd[F] = probeGuideConfigCmd(
    State.g4ProbeGuideConfigCmd,
    State.g4GuideConfig,
    TestTcsEvent.G4ProbeGuideConfig.apply
  )

  override val wavelG1: TargetWavelengthCmd[F] = new DummyCmd[F] with TargetWavelengthCmd[F] {
    override def setWavel(v: Double): F[Unit] = Applicative[F].unit
  }

  override val wavelG2: TargetWavelengthCmd[F] = new DummyCmd[F] with TargetWavelengthCmd[F] {
    override def setWavel(v: Double): F[Unit] = Applicative[F].unit
  }

  override val wavelG3: TargetWavelengthCmd[F] = new DummyCmd[F] with TargetWavelengthCmd[F] {
    override def setWavel(v: Double): F[Unit] = Applicative[F].unit
  }

  override val wavelG4: TargetWavelengthCmd[F] = new DummyCmd[F] with TargetWavelengthCmd[F] {
    override def setWavel(v: Double): F[Unit] = Applicative[F].unit
  }

  override def gwfs1Target: Target[F] = targetGetters(state, State.gwfs1Target.asGetter)

  override def gwfs2Target: Target[F] = targetGetters(state, State.gwfs2Target.asGetter)

  override def gwfs3Target: Target[F] = targetGetters(state, State.gwfs3Target.asGetter)

  override def gwfs4Target: Target[F] = targetGetters(state, State.gwfs4Target.asGetter)

  override val cwfs1FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.cwfs1FollowCmd,
    State.cwfs1Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.cwfs1Parked,
    TestTcsEvent.Cwfs1ProbeFollowCmd
  )

  override val cwfs2FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.cwfs2FollowCmd,
    State.cwfs2Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.cwfs2Parked,
    TestTcsEvent.Cwfs2ProbeFollowCmd
  )

  override val cwfs3FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.cwfs3FollowCmd,
    State.cwfs3Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.cwfs3Parked,
    TestTcsEvent.Cwfs3ProbeFollowCmd
  )

  override val odgw1FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.odgw1FollowCmd,
    State.odgw1Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.odgw1Parked,
    TestTcsEvent.Odgw1ProbeFollowCmd
  )

  override val odgw2FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.odgw2FollowCmd,
    State.odgw2Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.odgw2Parked,
    TestTcsEvent.Odgw2ProbeFollowCmd
  )

  override val odgw3FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.odgw3FollowCmd,
    State.odgw3Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.odgw3Parked,
    TestTcsEvent.Odgw3ProbeFollowCmd
  )

  override val odgw4FollowCmd: ProbeFollowCmd[F] = probeFollowCmd(
    State.odgw4FollowCmd,
    State.odgw4Follow.andThen(Lens[Boolean, String](_.fold("On", "Off"))(x => _ => x === "On")),
    State.odgw4Parked,
    TestTcsEvent.Odgw4ProbeFollowCmd
  )

  override val cwfs1ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.ngsPr1ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Cwfs1ParkCmd

      override protected def cmd(st: State): State =
        st.copy(cwfs1Follow = false, cwfs1Parked = true)
    }

  override val cwfs2ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.ngsPr2ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Cwfs2ParkCmd

      override protected def cmd(st: State): State =
        st.copy(cwfs2Follow = false, cwfs2Parked = true)
    }

  override val cwfs3ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.ngsPr3ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Cwfs3ParkCmd

      override protected def cmd(st: State): State =
        st.copy(cwfs3Follow = false, cwfs3Parked = true)
    }

  override val odgw1ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.odgw1ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Odgw1ParkCmd

      override protected def cmd(st: State): State = st.copy(odgw1Follow = false, p2Parked = true)
    }

  override val odgw2ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.odgw2ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Odgw2ParkCmd

      override protected def cmd(st: State): State = st.copy(odgw2Follow = false, p2Parked = true)
    }

  override val odgw3ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.odgw3ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Odgw3ParkCmd

      override protected def cmd(st: State): State = st.copy(odgw3Follow = false, p2Parked = true)
    }

  override val odgw4ParkCmd: EpicsCommand[F] =
    new TestEpicsCommand0[F, State, TestTcsEvent](State.odgw4ParkCmd, state, out) {
      override protected def event(st: State): TestTcsEvent = TestTcsEvent.Odgw4ParkCmd

      override protected def cmd(st: State): State = st.copy(odgw4Follow = false, p2Parked = true)
    }

  override def cwfs1Follow: F[Boolean] = state.get.map(_.cwfs1Follow)

  override def cwfs2Follow: F[Boolean] = state.get.map(_.cwfs2Follow)

  override def cwfs3Follow: F[Boolean] = state.get.map(_.cwfs3Follow)

  override def odgw1Follow: F[Boolean] = state.get.map(_.odgw1Follow)

  override def odgw2Follow: F[Boolean] = state.get.map(_.odgw2Follow)

  override def odgw3Follow: F[Boolean] = state.get.map(_.odgw3Follow)

  override def odgw4Follow: F[Boolean] = state.get.map(_.odgw4Follow)

  override def cwfs1Parked: F[Boolean] = state.get.map(_.cwfs1Parked)

  override def cwfs2Parked: F[Boolean] = state.get.map(_.cwfs2Parked)

  override def cwfs3Parked: F[Boolean] = state.get.map(_.cwfs3Parked)

  override def odgw1Parked: F[Boolean] = state.get.map(_.odgw1Parked)

  override def odgw2Parked: F[Boolean] = state.get.map(_.odgw2Parked)

  override def odgw3Parked: F[Boolean] = state.get.map(_.odgw3Parked)

  override def odgw4Parked: F[Boolean] = state.get.map(_.odgw4Parked)

  override def g1MapName: F[Option[GemsSource]] = state.get.map(_.g1MapName)

  override def g2MapName: F[Option[GemsSource]] = state.get.map(_.g2MapName)

  override def g3MapName: F[Option[GemsSource]] = state.get.map(_.g3MapName)

  override def g4MapName: F[Option[GemsSource]] = state.get.map(_.g4MapName)

  override def g1Wavelength: F[Double] = state.get.map(_.g1Wavelength)

  override def g2Wavelength: F[Double] = state.get.map(_.g2Wavelength)

  override def g3Wavelength: F[Double] = state.get.map(_.g3Wavelength)

  override def g4Wavelength: F[Double] = state.get.map(_.g4Wavelength)

  override val g1GuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.g1GuideConfig.asGetter)
  override val g2GuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.g2GuideConfig.asGetter)
  override val g3GuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.g3GuideConfig.asGetter)
  override val g4GuideConfig: ProbeGuideConfig[F] =
    probeGuideConfigGetters(state, State.g4GuideConfig.asGetter)

  def p2adec: F[Double] = state.get.map(_.p2adec)

  def p2ara: F[Double] = state.get.map(_.p2ara)

  def p2arv: F[Double] = state.get.map(_.p2arv)

  def p2awavel: F[Double] = state.get.map(_.p2awavel)

  def p2aepoch: F[String] = state.get.map(_.p2aepoch)

  def p2aequin: F[String] = state.get.map(_.p2aequin)

  def p2aframe: F[String] = state.get.map(_.p2aframe)

  def p2aobject: F[String] = state.get.map(_.p2aobject)

  def p2apmdec: F[Double] = state.get.map(_.p2apmdec)

  def p2apmra: F[Double] = state.get.map(_.p2apmra)

  def p2aparal: F[Double] = state.get.map(_.p2aparal)

  def p2observeB: F[Double] = state.get.map(_.p2observeB)

  private def probeGuideConfigCmd(
    cmdL:      Lens[State, TestEpicsCommand4.State[String, String, String, String]],
    statusL:   Lens[State, ProbeGuideConfigVals],
    evBuilder: (String, String, String, String) => TestTcsEvent
  ): ProbeGuideCmd[F] =
    new TestEpicsCommand4[F, State, TestTcsEvent, String, String, String, String](cmdL, state, out)
      with ProbeGuideCmd[F] {
      override def setNodachopa(v: String): F[Unit] = setParameter1(v)

      override def setNodachopb(v: String): F[Unit] = setParameter2(v)

      override def setNodbchopa(v: String): F[Unit] = setParameter3(v)

      override def setNodbchopb(v: String): F[Unit] = setParameter4(v)

      override protected def event(st: State): TestTcsEvent = evBuilder(
        cmdL.get(st).param1,
        cmdL.get(st).param2,
        cmdL.get(st).param3,
        cmdL.get(st).param4
      )

      override protected def cmd(st: State): State = statusL.replace(
        ProbeGuideConfigVals(
          if (cmdL.get(st).param1 == "On") 1 else 0,
          if (cmdL.get(st).param2 == "On") 1 else 0,
          if (cmdL.get(st).param3 == "On") 1 else 0,
          if (cmdL.get(st).param4 == "On") 1 else 0
        )
      )(st)
    }

  private def probeFollowCmd(
    cmdL:      Lens[State, TestEpicsCommand1.State[String]],
    statusL:   Lens[State, String],
    parkL:     Lens[State, Boolean],
    evBuilder: String => TestTcsEvent
  ): ProbeFollowCmd[F] =
    new TestEpicsCommand1[F, State, TestTcsEvent, String](cmdL, state, out) with ProbeFollowCmd[F] {
      override def setFollowState(v: String): F[Unit] = setParameter1(v)

      override protected def event(st: State): TestTcsEvent = evBuilder(cmdL.get(st).param1)

      override protected def cmd(st: State): State =
        (statusL.replace(cmdL.get(st).param1) >>> parkL.modify { v =>
          if (cmdL.get(st).param1 === "On") false else v
        })(st)
    }

  private def wfsObserveCmd(
    cmdL:    Lens[State, TestEpicsCommand1.State[Int]],
    statusL: Lens[State, BinaryYesNo],
    ev:      TestTcsEvent
  ): WfsObserveCmd[F] =
    new TestEpicsCommand1[F, State, TestTcsEvent, Int](cmdL, state, out) with WfsObserveCmd[F] {
      override protected def event(st: State): TestTcsEvent = ev

      override protected def cmd(st: State): State = statusL.replace(BinaryYesNo.Yes)(st)

      override def setNoexp(v: Integer): F[Unit] = setParameter1(v.toInt)

      override def setInt(v: Double): F[Unit] = Applicative[F].unit

      override def setOutopt(v: String): F[Unit] = Applicative[F].unit

      override def setLabel(v: String): F[Unit] = Applicative[F].unit

      override def setOutput(v: String): F[Unit] = Applicative[F].unit

      override def setPath(v: String): F[Unit] = Applicative[F].unit

      override def setName(v: String): F[Unit] = Applicative[F].unit
    }

}

object TestTcsEpics {

  @Lenses
  case class State(
    absorbTipTilt:             Int,
    m1GuideSource:             String,
    m1Guide:                   BinaryOnOff,
    m2p1Guide:                 String,
    m2p2Guide:                 String,
    m2oiGuide:                 String,
    m2aoGuide:                 String,
    comaCorrect:               String,
    m2GuideState:              BinaryOnOff,
    xoffsetPoA1:               Double,
    yoffsetPoA1:               Double,
    xoffsetPoB1:               Double,
    yoffsetPoB1:               Double,
    xoffsetPoC1:               Double,
    yoffsetPoC1:               Double,
    sourceAWavelength:         Double,
    sourceBWavelength:         Double,
    sourceCWavelength:         Double,
    chopBeam:                  String,
    p1FollowS:                 String,
    p2FollowS:                 String,
    oiFollowS:                 String,
    aoFollowS:                 String,
    p1Parked:                  Boolean,
    p2Parked:                  Boolean,
    oiParked:                  Boolean,
    aoParked:                  Boolean,
    oiName:                    String,
    pwfs1On:                   BinaryYesNo,
    pwfs2On:                   BinaryYesNo,
    oiwfsOn:                   BinaryYesNo,
    sfName:                    String,
    sfParked:                  Int,
    agHwName:                  String,
    agHwParked:                Int,
    instrAA:                   Double,
    inPosition:                String,
    agInPosition:              Double,
    pwfs1ProbeGuideConfig:     ProbeGuideConfigVals,
    pwfs2ProbeGuideConfig:     ProbeGuideConfigVals,
    oiwfsProbeGuideConfig:     ProbeGuideConfigVals,
    hourAngle:                 String,
    localTime:                 String,
    trackingFrame:             String,
    trackingEpoch:             Double,
    equinox:                   Double,
    trackingEquinox:           String,
    trackingDec:               Double,
    trackingRA:                Double,
    elevation:                 Double,
    azimuth:                   Double,
    crPositionAngle:           Double,
    ut:                        String,
    date:                      String,
    m2Baffle:                  String,
    m2CentralBaffle:           String,
    st:                        String,
    sfRotation:                Double,
    sfTilt:                    Double,
    sfLinear:                  Double,
    instrPA:                   Double,
    targetA:                   List[Double],
    aoFoldPosition:            String,
    useAo:                     BinaryYesNo,
    airmass:                   Double,
    airmassStart:              Double,
    airmassEnd:                Double,
    carouselMode:              String,
    crFollow:                  Int,
    crTrackingFrame:           String,
    sourceATarget:             TargetVals,
    pwfs1Target:               TargetVals,
    pwfs2Target:               TargetVals,
    oiwfsTarget:               TargetVals,
    parallacticAngle:          Angle,
    m2UserFocusOffset:         Double,
    pwfs1IntegrationTime:      Double,
    pwfs2IntegrationTime:      Double,
    oiwfsIntegrationTime:      Double,
    gsaoiPort:                 Int,
    gpiPort:                   Int,
    f2Port:                    Int,
    niriPort:                  Int,
    gnirsPort:                 Int,
    nifsPort:                  Int,
    gmosPort:                  Int,
    ghostPort:                 Int,
    igrins2Port:               Int,
    aoGuideStarX:              Double,
    aoGuideStarY:              Double,
    aoPreparedCMX:             Double,
    aoPreparedCMY:             Double,
    gwfs1Target:               TargetVals,
    gwfs2Target:               TargetVals,
    gwfs3Target:               TargetVals,
    gwfs4Target:               TargetVals,
    cwfs1Follow:               Boolean,
    cwfs2Follow:               Boolean,
    cwfs3Follow:               Boolean,
    odgw1Follow:               Boolean,
    odgw2Follow:               Boolean,
    odgw3Follow:               Boolean,
    odgw4Follow:               Boolean,
    cwfs1Parked:               Boolean,
    cwfs2Parked:               Boolean,
    cwfs3Parked:               Boolean,
    odgw1Parked:               Boolean,
    odgw2Parked:               Boolean,
    odgw3Parked:               Boolean,
    odgw4Parked:               Boolean,
    g1MapName:                 Option[GemsSource],
    g2MapName:                 Option[GemsSource],
    g3MapName:                 Option[GemsSource],
    g4MapName:                 Option[GemsSource],
    g1Wavelength:              Double,
    g2Wavelength:              Double,
    g3Wavelength:              Double,
    g4Wavelength:              Double,
    g1GuideConfig:             ProbeGuideConfigVals,
    g2GuideConfig:             ProbeGuideConfigVals,
    g3GuideConfig:             ProbeGuideConfigVals,
    g4GuideConfig:             ProbeGuideConfigVals,
    aoCorrect:                 String,
    aoGains:                   Int,
    defocusA:                  Double,
    defocusB:                  Double,
    m1GuideCmd:                TestEpicsCommand1.State[String],
    m2GuideCmd:                TestEpicsCommand1.State[String],
    m2GuideModeCmd:            TestEpicsCommand1.State[String],
    m2GuideConfigCmd:          TestEpicsCommand3.State[String, String, String],
    mountGuideCmd:             TestEpicsCommand2.State[String, String],
    pwfs1ProbeGuideConfigCmd:  TestEpicsCommand4.State[String, String, String, String],
    pwfs2ProbeGuideConfigCmd:  TestEpicsCommand4.State[String, String, String, String],
    oiwfsProbeGuideConfigCmd:  TestEpicsCommand4.State[String, String, String, String],
    pwfs1ProbeFollowCmd:       TestEpicsCommand1.State[String],
    pwfs2ProbeFollowCmd:       TestEpicsCommand1.State[String],
    oiwfsProbeFollowCmd:       TestEpicsCommand1.State[String],
    offsetACmd:                TestEpicsCommand2.State[Double, Double],
    wavelSourceACmd:           TestEpicsCommand1.State[Double],
    pwfs1ParkCmd:              TestEpicsCommand0.State,
    pwfs2ParkCmd:              TestEpicsCommand0.State,
    oiwfsParkCmd:              TestEpicsCommand0.State,
    pwfs1ObserveCmd:           TestEpicsCommand1.State[Int],
    pwfs2ObserveCmd:           TestEpicsCommand1.State[Int],
    oiwfsObserveCmd:           TestEpicsCommand1.State[Int],
    pwfs1StopObserveCmd:       TestEpicsCommand0.State,
    pwfs2StopObserveCmd:       TestEpicsCommand0.State,
    oiwfsStopObserveCmd:       TestEpicsCommand0.State,
    aoProbeFollowCmd:          TestEpicsCommand1.State[String],
    aoCorrectCmd:              TestEpicsCommand2.State[String, Int],
    aoPrepareControlMatrixCmd: TestEpicsCommand2.State[Double, Double],
    g1ProbeGuideConfigCmd:     TestEpicsCommand4.State[String, String, String, String],
    g2ProbeGuideConfigCmd:     TestEpicsCommand4.State[String, String, String, String],
    g3ProbeGuideConfigCmd:     TestEpicsCommand4.State[String, String, String, String],
    g4ProbeGuideConfigCmd:     TestEpicsCommand4.State[String, String, String, String],
    ngsPr1ParkCmd:             TestEpicsCommand0.State,
    ngsPr2ParkCmd:             TestEpicsCommand0.State,
    ngsPr3ParkCmd:             TestEpicsCommand0.State,
    odgw1ParkCmd:              TestEpicsCommand0.State,
    odgw2ParkCmd:              TestEpicsCommand0.State,
    odgw3ParkCmd:              TestEpicsCommand0.State,
    odgw4ParkCmd:              TestEpicsCommand0.State,
    cwfs1FollowCmd:            TestEpicsCommand1.State[String],
    cwfs2FollowCmd:            TestEpicsCommand1.State[String],
    cwfs3FollowCmd:            TestEpicsCommand1.State[String],
    odgw1FollowCmd:            TestEpicsCommand1.State[String],
    odgw2FollowCmd:            TestEpicsCommand1.State[String],
    odgw3FollowCmd:            TestEpicsCommand1.State[String],
    odgw4FollowCmd:            TestEpicsCommand1.State[String],
    p2ara:                     Double,
    p2adec:                    Double,
    p2arv:                     Double,
    p2awavel:                  Double,
    p2aepoch:                  String,
    p2aequin:                  String,
    p2aframe:                  String,
    p2aobject:                 String,
    p2apmra:                   Double,
    p2apmdec:                  Double,
    p2aparal:                  Double,
    p2observeB:                Double
  )

  @Lenses
  final case class ProbeGuideConfigVals(
    nodachopa: Int,
    nodachopb: Int,
    nodbchopa: Int,
    nodbchopb: Int
  )

  object ProbeGuideConfigVals {
    val default: ProbeGuideConfigVals = ProbeGuideConfigVals(0, 0, 0, 0)
  }

  @Lenses
  final case class TargetVals(
    objectName:        String,
    ra:                Double,
    dec:               Double,
    frame:             String,
    equinox:           String,
    epoch:             String,
    properMotionRA:    Double,
    properMotionDec:   Double,
    centralWavelenght: Double,
    parallax:          Double,
    radialVelocity:    Double
  )

  object TargetVals {
    val default: TargetVals = TargetVals(
      objectName = "",
      ra = 0.0,
      dec = 0.0,
      frame = "",
      equinox = "",
      epoch = "",
      properMotionRA = 0.0,
      properMotionDec = 0.0,
      centralWavelenght = 0.0,
      parallax = 0.0,
      radialVelocity = 0.0
    )
  }

  def probeGuideConfigGetters[F[_]: Applicative](
    st: Ref[F, State],
    g:  Getter[State, ProbeGuideConfigVals]
  ): ProbeGuideConfig[F] =
    new ProbeGuideConfig[F] {
      override def nodachopa: F[Int] = st.get.map(g.andThen(ProbeGuideConfigVals.nodachopa).get)
      override def nodachopb: F[Int] = st.get.map(g.andThen(ProbeGuideConfigVals.nodachopb).get)
      override def nodbchopa: F[Int] = st.get.map(g.andThen(ProbeGuideConfigVals.nodbchopa).get)
      override def nodbchopb: F[Int] = st.get.map(g.andThen(ProbeGuideConfigVals.nodbchopb).get)
    }

  def targetGetters[F[_]: Applicative](st: Ref[F, State], g: Getter[State, TargetVals]): Target[F] =
    new Target[F] {
      override def objectName: F[String]        = st.get.map(g.andThen(TargetVals.objectName).get)
      override def ra: F[Double]                = st.get.map(g.andThen(TargetVals.ra).get)
      override def dec: F[Double]               = st.get.map(g.andThen(TargetVals.dec).get)
      override def frame: F[String]             = st.get.map(g.andThen(TargetVals.frame).get)
      override def equinox: F[String]           = st.get.map(g.andThen(TargetVals.equinox).get)
      override def epoch: F[String]             = st.get.map(g.andThen(TargetVals.epoch).get)
      override def properMotionRA: F[Double]    = st.get.map(g.andThen(TargetVals.properMotionRA).get)
      override def properMotionDec: F[Double]   =
        st.get.map(g.andThen(TargetVals.properMotionDec).get)
      override def centralWavelenght: F[Double] =
        st.get.map(g.andThen(TargetVals.centralWavelenght).get)
      override def parallax: F[Double]          = st.get.map(g.andThen(TargetVals.parallax).get)
      override def radialVelocity: F[Double]    = st.get.map(g.andThen(TargetVals.radialVelocity).get)
    }

  sealed trait TestTcsEvent extends Product with Serializable
  object TestTcsEvent {
    final case class M1GuideCmd(newState: String)                  extends TestTcsEvent
    final case class M2GuideCmd(newState: String)                  extends TestTcsEvent
    final case class M2GuideModeCmd(newComaState: String)          extends TestTcsEvent
    final case class M2GuideConfigCmd(source: String, beam: String, reset: String)
        extends TestTcsEvent
    final case class MountGuideCmd(source: String, mode: String)   extends TestTcsEvent
    final case class Pwfs1ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class Pwfs2ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class OiwfsProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class G1ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class G2ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class G3ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class G4ProbeGuideConfig(
      nodachopa: String,
      nodchopb:  String,
      nodbchopa: String,
      nodbchopb: String
    ) extends TestTcsEvent
    final case class OffsetACmd(p: Double, q: Double)              extends TestTcsEvent
    final case class WavelSourceACmd(w: Double)                    extends TestTcsEvent
    final case class Pwfs1ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Pwfs2ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class OiwfsProbeFollowCmd(state: String)            extends TestTcsEvent
    case object Pwfs1ParkCmd                                       extends TestTcsEvent
    case object Pwfs2ParkCmd                                       extends TestTcsEvent
    case object OiwfsParkCmd                                       extends TestTcsEvent
    case object Pwfs1ObserveCmd                                    extends TestTcsEvent
    case object Pwfs2ObserveCmd                                    extends TestTcsEvent
    case object OiwfsObserveCmd                                    extends TestTcsEvent
    case object Pwfs1StopObserveCmd                                extends TestTcsEvent
    case object Pwfs2StopObserveCmd                                extends TestTcsEvent
    case object OiwfsStopObserveCmd                                extends TestTcsEvent
    final case class AoProbeFollowCmd(state: String)               extends TestTcsEvent
    final case class AoCorrectCmd(correct: String, gains: Int)     extends TestTcsEvent
    final case class AoPrepareMatrix(aogsx: Double, aogsy: Double) extends TestTcsEvent
    final case class Cwfs1ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Cwfs2ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Cwfs3ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Odgw1ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Odgw2ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Odgw3ProbeFollowCmd(state: String)            extends TestTcsEvent
    final case class Odgw4ProbeFollowCmd(state: String)            extends TestTcsEvent
    case object Cwfs1ParkCmd                                       extends TestTcsEvent
    case object Cwfs2ParkCmd                                       extends TestTcsEvent
    case object Cwfs3ParkCmd                                       extends TestTcsEvent
    case object Odgw1ParkCmd                                       extends TestTcsEvent
    case object Odgw2ParkCmd                                       extends TestTcsEvent
    case object Odgw3ParkCmd                                       extends TestTcsEvent
    case object Odgw4ParkCmd                                       extends TestTcsEvent

    implicit val eqTestTcsEvPent: Eq[TestTcsEvent] = Eq.instance {
      case (Pwfs1ParkCmd, Pwfs1ParkCmd)                                           => true
      case (Pwfs2ParkCmd, Pwfs2ParkCmd)                                           => true
      case (OiwfsParkCmd, OiwfsParkCmd)                                           => true
      case (Pwfs1ObserveCmd, Pwfs1ObserveCmd)                                     => true
      case (Pwfs2ObserveCmd, Pwfs2ObserveCmd)                                     => true
      case (OiwfsObserveCmd, OiwfsObserveCmd)                                     => true
      case (Pwfs1StopObserveCmd, Pwfs1StopObserveCmd)                             => true
      case (Pwfs2StopObserveCmd, Pwfs2StopObserveCmd)                             => true
      case (OiwfsStopObserveCmd, OiwfsStopObserveCmd)                             => true
      case (M1GuideCmd(a), M1GuideCmd(x))                                         => a === x
      case (M2GuideCmd(a), M2GuideCmd(x))                                         => a === x
      case (M2GuideModeCmd(a), M2GuideModeCmd(x))                                 => a === x
      case (Pwfs1ProbeFollowCmd(a), Pwfs1ProbeFollowCmd(x))                       => a === x
      case (Pwfs2ProbeFollowCmd(a), Pwfs2ProbeFollowCmd(x))                       => a === x
      case (OiwfsProbeFollowCmd(a), OiwfsProbeFollowCmd(x))                       => a === x
      case (OffsetACmd(a, b), OffsetACmd(x, y))                                   => a === x && b === y
      case (MountGuideCmd(a, b), MountGuideCmd(x, y))                             => a === x && b === y
      case (M2GuideConfigCmd(a, b, c), M2GuideConfigCmd(x, y, z))                 => a === x && b === y && c === z
      case (Pwfs1ProbeGuideConfig(a, b, c, d), Pwfs1ProbeGuideConfig(x, y, z, w)) =>
        a === x && b === y && c === z && d === w
      case (Pwfs2ProbeGuideConfig(a, b, c, d), Pwfs2ProbeGuideConfig(x, y, z, w)) =>
        a === x && b === y && c === z && d === w
      case (OiwfsProbeGuideConfig(a, b, c, d), OiwfsProbeGuideConfig(x, y, z, w)) =>
        a === x && b === y && c === z && d === w
      case (G1ProbeGuideConfig(a, b, c, d), G1ProbeGuideConfig(x, y, z, w))       =>
        a === x && b === y && c === z && d === w
      case (G2ProbeGuideConfig(a, b, c, d), G2ProbeGuideConfig(x, y, z, w))       =>
        a === x && b === y && c === z && d === w
      case (G3ProbeGuideConfig(a, b, c, d), G3ProbeGuideConfig(x, y, z, w))       =>
        a === x && b === y && c === z && d === w
      case (G4ProbeGuideConfig(a, b, c, d), G4ProbeGuideConfig(x, y, z, w))       =>
        a === x && b === y && c === z && d === w
      case (Cwfs1ProbeFollowCmd(a), Cwfs1ProbeFollowCmd(b))                       => a === b
      case (Cwfs2ProbeFollowCmd(a), Cwfs2ProbeFollowCmd(b))                       => a === b
      case (Cwfs3ProbeFollowCmd(a), Cwfs3ProbeFollowCmd(b))                       => a === b
      case (Odgw1ProbeFollowCmd(a), Odgw1ProbeFollowCmd(b))                       => a === b
      case (Odgw2ProbeFollowCmd(a), Odgw2ProbeFollowCmd(b))                       => a === b
      case (Odgw3ProbeFollowCmd(a), Odgw3ProbeFollowCmd(b))                       => a === b
      case (Odgw4ProbeFollowCmd(a), Odgw4ProbeFollowCmd(b))                       => a === b
      case (Cwfs1ParkCmd, Cwfs1ParkCmd)                                           => true
      case (Cwfs2ParkCmd, Cwfs2ParkCmd)                                           => true
      case (Cwfs3ParkCmd, Cwfs3ParkCmd)                                           => true
      case (Odgw1ParkCmd, Odgw1ParkCmd)                                           => true
      case (Odgw2ParkCmd, Odgw2ParkCmd)                                           => true
      case (Odgw3ParkCmd, Odgw3ParkCmd)                                           => true
      case (Odgw4ParkCmd, Odgw4ParkCmd)                                           => true
      case _                                                                      => false
    }

  }

  val defaultState: State = State(
    absorbTipTilt = 0,
    m1GuideSource = "",
    m1Guide = BinaryOnOff.Off,
    m2p1Guide = "OFF",
    m2p2Guide = "OFF",
    m2oiGuide = "OFF",
    m2aoGuide = "OFF",
    comaCorrect = "Off",
    m2GuideState = BinaryOnOff.Off,
    xoffsetPoA1 = 0.0,
    yoffsetPoA1 = 0.0,
    xoffsetPoB1 = 0.0,
    yoffsetPoB1 = 0.0,
    xoffsetPoC1 = 0.0,
    yoffsetPoC1 = 0.0,
    sourceAWavelength = 0.0,
    sourceBWavelength = 0.0,
    sourceCWavelength = 0.0,
    chopBeam = "Off",
    p1FollowS = "Off",
    p2FollowS = "Off",
    oiFollowS = "Off",
    aoFollowS = "Off",
    p1Parked = true,
    p2Parked = true,
    oiParked = true,
    aoParked = true,
    oiName = "None",
    pwfs1On = BinaryYesNo.No,
    pwfs2On = BinaryYesNo.No,
    oiwfsOn = BinaryYesNo.No,
    sfName = "",
    sfParked = 0,
    agHwName = "",
    agHwParked = 0,
    instrAA = 0.0,
    inPosition = "",
    agInPosition = 0.0,
    pwfs1ProbeGuideConfig = ProbeGuideConfigVals.default,
    pwfs2ProbeGuideConfig = ProbeGuideConfigVals.default,
    oiwfsProbeGuideConfig = ProbeGuideConfigVals.default,
    hourAngle = "",
    localTime = "",
    trackingFrame = "",
    trackingEpoch = 0.0,
    equinox = 0.0,
    trackingEquinox = "",
    trackingDec = 0.0,
    trackingRA = 0.0,
    elevation = 0.0,
    azimuth = 0.0,
    crPositionAngle = 0.0,
    ut = "",
    date = "",
    m2Baffle = "Off",
    m2CentralBaffle = "Off",
    st = "",
    sfRotation = 0.0,
    sfTilt = 0.0,
    sfLinear = 0.0,
    instrPA = 0.0,
    targetA = List.empty,
    aoFoldPosition = "OUT",
    useAo = BinaryYesNo.No,
    airmass = 0.0,
    airmassStart = 0.0,
    airmassEnd = 0.0,
    carouselMode = "",
    crFollow = 0,
    crTrackingFrame = "",
    sourceATarget = TargetVals.default,
    pwfs1Target = TargetVals.default,
    pwfs2Target = TargetVals.default,
    oiwfsTarget = TargetVals.default,
    parallacticAngle = 0.0.radians,
    m2UserFocusOffset = 0.0,
    pwfs1IntegrationTime = 0.0,
    pwfs2IntegrationTime = 0.0,
    oiwfsIntegrationTime = 0.0,
    gsaoiPort = 0,
    gpiPort = 0,
    f2Port = 0,
    niriPort = 0,
    gnirsPort = 0,
    nifsPort = 0,
    gmosPort = 0,
    ghostPort = 0,
    igrins2Port = 0,
    aoGuideStarX = 0.0,
    aoGuideStarY = 0.0,
    aoPreparedCMX = 0.0,
    aoPreparedCMY = 0.0,
    gwfs1Target = TargetVals.default,
    gwfs2Target = TargetVals.default,
    gwfs3Target = TargetVals.default,
    gwfs4Target = TargetVals.default,
    cwfs1Follow = false,
    cwfs2Follow = false,
    cwfs3Follow = false,
    odgw1Follow = false,
    odgw2Follow = false,
    odgw3Follow = false,
    odgw4Follow = false,
    cwfs1Parked = true,
    cwfs2Parked = true,
    cwfs3Parked = true,
    odgw1Parked = true,
    odgw2Parked = true,
    odgw3Parked = true,
    odgw4Parked = true,
    g1MapName = none,
    g2MapName = none,
    g3MapName = none,
    g4MapName = none,
    g1Wavelength = 0.0,
    g2Wavelength = 0.0,
    g3Wavelength = 0.0,
    g4Wavelength = 0.0,
    g1GuideConfig = ProbeGuideConfigVals.default,
    g2GuideConfig = ProbeGuideConfigVals.default,
    g3GuideConfig = ProbeGuideConfigVals.default,
    g4GuideConfig = ProbeGuideConfigVals.default,
    aoCorrect = "Off",
    aoGains = 0,
    defocusA = 0.0,
    defocusB = 0.0,
    m1GuideCmd = TestEpicsCommand1.State[String](mark = false, ""),
    m2GuideCmd = TestEpicsCommand1.State[String](mark = false, ""),
    m2GuideModeCmd = TestEpicsCommand1.State[String](mark = false, ""),
    m2GuideConfigCmd = TestEpicsCommand3.State[String, String, String](mark = false, "", "", ""),
    mountGuideCmd = TestEpicsCommand2.State[String, String](mark = false, "", ""),
    pwfs1ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    pwfs2ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    oiwfsProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    pwfs1ProbeFollowCmd = TestEpicsCommand1.State[String](mark = false, ""),
    pwfs2ProbeFollowCmd = TestEpicsCommand1.State[String](mark = false, ""),
    oiwfsProbeFollowCmd = TestEpicsCommand1.State[String](mark = false, ""),
    offsetACmd = TestEpicsCommand2.State[Double, Double](mark = false, 0.0, 0.0),
    wavelSourceACmd = TestEpicsCommand1.State[Double](mark = false, 0.0),
    pwfs1ParkCmd = false,
    pwfs2ParkCmd = false,
    oiwfsParkCmd = false,
    pwfs1ObserveCmd = TestEpicsCommand1.State[Int](mark = false, -1),
    pwfs2ObserveCmd = TestEpicsCommand1.State[Int](mark = false, -1),
    oiwfsObserveCmd = TestEpicsCommand1.State[Int](mark = false, -1),
    pwfs1StopObserveCmd = false,
    pwfs2StopObserveCmd = false,
    oiwfsStopObserveCmd = false,
    aoProbeFollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    aoCorrectCmd = TestEpicsCommand2.State[String, Int](mark = false, "OFF", 0),
    aoPrepareControlMatrixCmd = TestEpicsCommand2.State[Double, Double](mark = false, 0.0, 0.0),
    g1ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    g2ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    g3ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    g4ProbeGuideConfigCmd =
      TestEpicsCommand4.State[String, String, String, String](mark = false, "", "", "", ""),
    ngsPr1ParkCmd = false,
    ngsPr2ParkCmd = false,
    ngsPr3ParkCmd = false,
    odgw1ParkCmd = false,
    odgw2ParkCmd = false,
    odgw3ParkCmd = false,
    odgw4ParkCmd = false,
    cwfs1FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    cwfs2FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    cwfs3FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    odgw1FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    odgw2FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    odgw3FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    odgw4FollowCmd = TestEpicsCommand1.State[String](mark = false, "Off"),
    p2ara = 0,
    p2adec = 0,
    p2arv = 0,
    p2awavel = 0,
    p2aepoch = "J2000",
    p2aequin = "000",
    p2aframe = "FK5",
    p2aobject = "GAIA 123",
    p2apmdec = 0,
    p2apmra = 0,
    p2aparal = 0,
    p2observeB = 0
  )

  def build[F[_]: Async](baseState: TestTcsEpics.State): F[TestTcsEpics[F]] =
    for {
      stR  <- Ref.of[F, TestTcsEpics.State](baseState)
      outR <- Ref.of[F, List[TestTcsEpics.TestTcsEvent]](List.empty)
    } yield TestTcsEpics[F](stR, outR)

}
