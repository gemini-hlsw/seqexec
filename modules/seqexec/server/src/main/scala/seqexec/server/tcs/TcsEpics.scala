// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import java.util.concurrent.TimeUnit

import cats.effect.{Async, IO, LiftIO, Sync, Timer}
import cats.implicits._
import squants.Angle
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.tcs.{BinaryEnabledDisabled, BinaryOnOff, BinaryYesNo}
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.EpicsCommandBase._
import seqexec.server.EpicsUtil._
import seqexec.server.SeqexecFailure.SeqexecException
import seqexec.server.{EpicsCommand, EpicsCommandBase, EpicsSystem}
import squants.space.Degrees

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.time.Duration

import scala.concurrent.duration.FiniteDuration

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all the objects used
 * to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

trait TcsEpics[F[_]] {

  import TcsEpics._

  def post(timeout: FiniteDuration): F[ApplyCommandResult]

  val m1GuideCmd: M1GuideCmd[F]

  val m2GuideCmd: M2GuideCmd[F]

  val m2GuideModeCmd: M2GuideModeCmd[F]

  val m2GuideConfigCmd: M2GuideConfigCmd[F]

  val mountGuideCmd: MountGuideCmd[F]

  val offsetACmd: OffsetCmd[F]

  val offsetBCmd: OffsetCmd[F]

  val wavelSourceA: TargetWavelengthCmd[F]

  val wavelSourceB: TargetWavelengthCmd[F]

  val m2Beam: M2Beam[F]

  val pwfs1ProbeGuideCmd: ProbeGuideCmd[F]

  val pwfs2ProbeGuideCmd: ProbeGuideCmd[F]

  val oiwfsProbeGuideCmd: ProbeGuideCmd[F]

  val pwfs1ProbeFollowCmd: ProbeFollowCmd[F]

  val pwfs2ProbeFollowCmd: ProbeFollowCmd[F]

  val oiwfsProbeFollowCmd: ProbeFollowCmd[F]

  val aoProbeFollowCmd: ProbeFollowCmd[F]

  val pwfs1Park: EpicsCommand[F]

  val pwfs2Park: EpicsCommand[F]

  val oiwfsPark: EpicsCommand[F]

  val pwfs1StopObserveCmd: EpicsCommand[F]

  val pwfs2StopObserveCmd: EpicsCommand[F]

  val oiwfsStopObserveCmd: EpicsCommand[F]

  val pwfs1ObserveCmd: WfsObserveCmd[F]

  val pwfs2ObserveCmd: WfsObserveCmd[F]

  val oiwfsObserveCmd: WfsObserveCmd[F]

  val hrwfsParkCmd: EpicsCommand[F]

  val hrwfsPosCmd: HrwfsPosCmd[F]

  val scienceFoldParkCmd: EpicsCommand[F]

  val scienceFoldPosCmd: ScienceFoldPosCmd[F]

  val observe: EpicsCommand[F]

  val endObserve: EpicsCommand[F]

  val aoCorrect: AoCorrect[F]

  val aoPrepareControlMatrix: AoPrepareControlMatrix[F]

  val aoFlatten: EpicsCommand[F]

  val aoStatistics: AoStatistics[F]

  val targetFilter: TargetFilter[F]

  def absorbTipTilt: F[Int]

  def m1GuideSource: F[String]

  def m1Guide: F[BinaryOnOff]

  def m2p1Guide: F[String]

  def m2p2Guide: F[String]

  def m2oiGuide: F[String]

  def m2aoGuide: F[String]

  def comaCorrect: F[String]

  def m2GuideState: F[BinaryOnOff]

  def xoffsetPoA1: F[Double]

  def yoffsetPoA1: F[Double]

  def xoffsetPoB1: F[Double]

  def yoffsetPoB1: F[Double]

  def xoffsetPoC1: F[Double]

  def yoffsetPoC1: F[Double]

  def sourceAWavelength: F[Double]

  def sourceBWavelength: F[Double]

  def sourceCWavelength: F[Double]

  def chopBeam: F[String]

  def p1FollowS: F[String]

  def p2FollowS: F[String]

  def oiFollowS: F[String]

  def aoFollowS: F[String]

  def p1Parked: F[Boolean]

  def p2Parked: F[Boolean]

  def oiParked: F[Boolean]

  def pwfs1On: F[BinaryYesNo]

  def pwfs2On:F[BinaryYesNo]

  def oiwfsOn: F[BinaryYesNo]

  def sfName: F[String]

  def sfParked: F[Int]

  def agHwName: F[String]

  def agHwParked: F[Int]

  def instrAA: F[Double]

  def inPosition:F[String]

  def agInPosition:F[Double]

  val pwfs1ProbeGuideConfig: ProbeGuideConfig[F]

  val pwfs2ProbeGuideConfig: ProbeGuideConfig[F]

  val oiwfsProbeGuideConfig: ProbeGuideConfig[F]

  // This functions returns a F that, when run, first waits tcsSettleTime to absorb in-position transients, then waits
  // for the in-position to change to true and stay true for stabilizationTime. It will wait up to `timeout`
  // seconds for that to happen.
  def waitInPosition(stabilizationTime: Duration, timeout: FiniteDuration)(implicit T: Timer[F]): F[Unit]

  // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
  /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
   * an error. A better solution is to detect the edge, from not in position to in-position.
   */
  def waitAGInPosition(timeout: FiniteDuration)(implicit T: Timer[F]): F[Unit]

  def hourAngle: F[String]

  def localTime: F[String]

  def trackingFrame: F[String]

  def trackingEpoch: F[Double]

  def equinox: F[Double]

  def trackingEquinox: F[String]

  def trackingDec: F[Double]

  def trackingRA: F[Double]

  def elevation: F[Double]

  def azimuth: F[Double]

  def crPositionAngle: F[Double]

  def ut: F[String]

  def date: F[String]

  def m2Baffle: F[String]

  def m2CentralBaffle: F[String]

  def st: F[String]

  def sfRotation: F[Double]

  def sfTilt: F[Double]

  def sfLinear: F[Double]

  def instrPA: F[Double]

  def targetA: F[List[Double]]

  def aoFoldPosition: F[String]

  def useAo: F[BinaryYesNo]

  def airmass: F[Double]

  def airmassStart: F[Double]

  def airmassEnd: F[Double]

  def carouselMode: F[String]

  def crFollow: F[Int]

  def sourceATarget: Target[F]

  val pwfs1Target: Target[F]

  val pwfs2Target: Target[F]

  val oiwfsTarget: Target[F]

  def parallacticAngle: F[Angle]

  def m2UserFocusOffset: F[Double]

  def pwfs1IntegrationTime: F[Double]

  def pwfs2IntegrationTime: F[Double]

  // Attribute must be changed back to Double after EPICS channel is fixed.
  def oiwfsIntegrationTime:F[Double]

  def gsaoiPort: F[Int]

  def gpiPort: F[Int]

  def f2Port: F[Int]

  def niriPort: F[Int]

  def gnirsPort: F[Int]

  def nifsPort: F[Int]

  def gmosPort: F[Int]

  def ghostPort: F[Int]

  def aoGuideStarX: F[Double]

  def aoGuideStarY: F[Double]

  def aoPreparedCMX: F[Double]

  def aoPreparedCMY: F[Double]

  // GeMS Commands
  import VirtualGemsTelescope._

  val g1ProbeGuideCmd: ProbeGuideCmd[F]

  val g2ProbeGuideCmd: ProbeGuideCmd[F]

  val g3ProbeGuideCmd: ProbeGuideCmd[F]

  val g4ProbeGuideCmd: ProbeGuideCmd[F]

  def gemsProbeGuideCmd(g: VirtualGemsTelescope): ProbeGuideCmd[F] = g match {
    case G1 => g1ProbeGuideCmd
    case G2 => g2ProbeGuideCmd
    case G3 => g3ProbeGuideCmd
    case G4 => g4ProbeGuideCmd
  }

  val wavelG1: TargetWavelengthCmd[F]

  val wavelG2: TargetWavelengthCmd[F]

  val wavelG3: TargetWavelengthCmd[F]

  val wavelG4: TargetWavelengthCmd[F]

  def gemsWavelengthCmd(g: VirtualGemsTelescope): TargetWavelengthCmd[F] = g match {
    case G1 => wavelG1
    case G2 => wavelG2
    case G3 => wavelG3
    case G4 => wavelG4
  }

  def gwfs1Target: Target[F]

  def gwfs2Target: Target[F]

  def gwfs3Target: Target[F]

  def gwfs4Target: Target[F]

  def gemsTarget(g: VirtualGemsTelescope): Target[F] = g match {
    case G1 => gwfs1Target
    case G2 => gwfs2Target
    case G3 => gwfs3Target
    case G4 => gwfs4Target
  }

  val cwfs1ProbeFollowCmd: ProbeFollowCmd[F]

  val cwfs2ProbeFollowCmd: ProbeFollowCmd[F]

  val cwfs3ProbeFollowCmd: ProbeFollowCmd[F]

  val odgw1FollowCmd: ProbeFollowCmd[F]

  val odgw2FollowCmd: ProbeFollowCmd[F]

  val odgw3FollowCmd: ProbeFollowCmd[F]

  val odgw4FollowCmd: ProbeFollowCmd[F]

  val odgw1ParkCmd: EpicsCommand[F]

  val odgw2ParkCmd: EpicsCommand[F]

  val odgw3ParkCmd: EpicsCommand[F]

  val odgw4ParkCmd: EpicsCommand[F]

  // GeMS statuses

  def cwfs1Follow: F[Boolean]

  def cwfs2Follow: F[Boolean]

  def cwfs3Follow: F[Boolean]

  def odgw1Follow: F[Boolean]

  def odgw2Follow: F[Boolean]

  def odgw3Follow: F[Boolean]

  def odgw4Follow: F[Boolean]

  def odgw1Parked: F[Boolean]

  def odgw2Parked: F[Boolean]

  def odgw3Parked: F[Boolean]

  def odgw4Parked: F[Boolean]

  def g1MapName: F[Option[GemsSource]]

  def g2MapName: F[Option[GemsSource]]

  def g3MapName: F[Option[GemsSource]]

  def g4MapName: F[Option[GemsSource]]

  def g1Wavelength: F[Double]

  def g2Wavelength: F[Double]

  def g3Wavelength: F[Double]

  def g4Wavelength: F[Double]

  def gemsWavelength(g: VirtualGemsTelescope): F[Double] = g match {
    case G1 => g1Wavelength
    case G2 => g2Wavelength
    case G3 => g3Wavelength
    case G4 => g4Wavelength
  }

  val g1GuideConfig: ProbeGuideConfig[F]

  val g2GuideConfig: ProbeGuideConfig[F]

  val g3GuideConfig: ProbeGuideConfig[F]

  val g4GuideConfig: ProbeGuideConfig[F]

  def gemsGuideConfig(g: VirtualGemsTelescope): ProbeGuideConfig[F] = g match {
    case G1 => g1GuideConfig
    case G2 => g2GuideConfig
    case G3 => g3GuideConfig
    case G4 => g4GuideConfig
  }

}

final class TcsEpicsImpl[F[_]: Async](epicsService: CaService, tops: Map[String, String]) extends TcsEpics[F] {

  import TcsEpics._

  val TcsTop: String = tops.getOrElse("tcs", "")

  // This is a bit ugly. Commands are triggered from the main apply record, so I just choose an arbitrary command here.
  // Triggering that command will trigger all the marked commands.
  override def post(timeout: FiniteDuration): F[ApplyCommandResult] = m1GuideCmd.post(timeout)

  override val m1GuideCmd: M1GuideCmd[F] = new EpicsCommandBase[F] with M1GuideCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m1Guide"))
    private val state = cs.map(_.getString("state"))

    override def setState(v: String): F[Unit] = setParameter(state, v)
  }

  override val m2GuideCmd: M2GuideCmd[F] = new EpicsCommandBase[F] with M2GuideCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Guide"))
    private val state = cs.map(_.getString("state"))

    override def setState(v: String): F[Unit] = setParameter(state, v)
  }

  override val m2GuideModeCmd: M2GuideModeCmd[F] = new EpicsCommandBase[F] with M2GuideModeCmd[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideMode"))

    private val coma = cs.map(_.getString("coma"))
    override def setComa(v: String): F[Unit] = setParameter(coma, v)
  }

  override val m2GuideConfigCmd: M2GuideConfigCmd[F] = new EpicsCommandBase[F] with M2GuideConfigCmd[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideConfig"))

    private val source = cs.map(_.getString("source"))
    override def setSource(v: String): F[Unit] = setParameter(source, v)

    private val beam = cs.map(_.getString("beam"))
    override def setBeam(v: String): F[Unit] = setParameter(beam, v)

    private val reset = cs.map(_.getString("reset"))
    override def setReset(v: String): F[Unit] = setParameter(reset, v)
  }

  override val mountGuideCmd: MountGuideCmd[F] = new EpicsCommandBase[F] with MountGuideCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("mountGuide"))

    private val source = cs.map(_.getString("source"))

    override def setSource(v: String): F[Unit] = setParameter(source, v)

    private val p1weight = cs.map(_.getDouble("p1weight"))

    override def setP1Weight(v: Double): F[Unit] = setParameter[F, java.lang.Double](p1weight, v)

    private val p2weight = cs.map(_.getDouble("p2weight"))

    override def setP2Weight(v: Double): F[Unit] = setParameter[F, java.lang.Double](p2weight, v)

    private val mode = cs.map(_.getString("mode"))

    override def setMode(v: String): F[Unit] = setParameter(mode, v)
  }

  override val offsetACmd: OffsetCmd[F] = new EpicsCommandBase[F] with OffsetCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoA1"))

    private val x = cs.map(_.getDouble("x"))

    override def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    override def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)
  }

  override val offsetBCmd: OffsetCmd[F] = new EpicsCommandBase[F] with OffsetCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoB1"))

    private val x = cs.map(_.getDouble("x"))

    override def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    override def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)
  }

  override val wavelSourceA: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelSourceA", epicsService)

  override val wavelSourceB: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelSourceB", epicsService)

  override val m2Beam: M2Beam[F] = new EpicsCommandBase[F] with M2Beam[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Beam"))

    private val beam = cs.map(_.getString("beam"))

    override def setBeam(v: String): F[Unit] = setParameter(beam, v)
  }

  override val pwfs1ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("pwfs1Guide", epicsService)

  override val pwfs2ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("pwfs2Guide", epicsService)

  override val oiwfsProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("oiwfsGuide", epicsService)

  override val pwfs1ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("p1Follow", epicsService)

  override val pwfs2ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("p2Follow", epicsService)

  override val oiwfsProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("oiFollow", epicsService)

  override val aoProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("aoFollow", epicsService)

  override val pwfs1Park: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1Park"))
  }

  override val pwfs2Park: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2Park"))
  }

  override val oiwfsPark: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsPark"))
  }

  override val pwfs1StopObserveCmd: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1StopObserve"))
  }

  override val pwfs2StopObserveCmd: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2StopObserve"))
  }

  override val oiwfsStopObserveCmd: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsStopObserve"))
  }

  override val pwfs1ObserveCmd: WfsObserveCmd[F] = new WfsObserveCmdImpl("pwfs1Observe", epicsService)

  override val pwfs2ObserveCmd: WfsObserveCmd[F] = new WfsObserveCmdImpl("pwfs2Observe", epicsService)

  override val oiwfsObserveCmd: WfsObserveCmd[F] = new WfsObserveCmdImpl("oiwfsObserve", epicsService)

  override val hrwfsParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfsPark"))
  }

  override val hrwfsPosCmd: HrwfsPosCmd[F] = new EpicsCommandBase[F] with HrwfsPosCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfs"))

    private val hrwfsPos = cs.map(_.getString("hrwfsPos"))

    override def setHrwfsPos(v: String): F[Unit] = setParameter(hrwfsPos, v)
  }

  override val scienceFoldParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFoldPark"))
  }

  override val scienceFoldPosCmd: ScienceFoldPosCmd[F] = new EpicsCommandBase[F] with ScienceFoldPosCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFold"))

    private val scfold = cs.map(_.getString("scfold"))

    override def setScfold(v: String): F[Unit] = setParameter(scfold, v)
  }

  override val observe: EpicsCommand[F] = new EpicsCommandBase[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::observe"))
  }

  override val endObserve: EpicsCommand[F] = new EpicsCommandBase[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::endObserve"))
  }

  override val aoCorrect: AoCorrect[F] = new EpicsCommandBase[F] with AoCorrect[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoCorrect"))

    private val correct = cs.map(_.getString("correct"))
    override def setCorrections(v: String): F[Unit] = setParameter(correct, v)

    private val gains = cs.map(_.getInteger("gains"))
    override def setGains(v: Int): F[Unit] = setParameter[F, java.lang.Integer](gains, v)

    private val matrix = cs.map(_.getInteger("matrix"))
    override def setMatrix(v: Int): F[Unit] = setParameter[F, java.lang.Integer](matrix, v)
  }

  override val aoPrepareControlMatrix: AoPrepareControlMatrix[F] =
    new EpicsCommandBase[F] with AoPrepareControlMatrix[F] {
      override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoPrepareCm"))

      private val x = cs.map(_.getDouble("x"))
      override def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

      private val y = cs.map(_.getDouble("y"))
      override def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)

      private val seeing = cs.map(_.getDouble("seeing"))
      override def setSeeing(v: Double): F[Unit] = setParameter[F, java.lang.Double](seeing, v)

      private val starMagnitude = cs.map(_.getDouble("gsmag"))
      override def setStarMagnitude(v: Double): F[Unit] = setParameter[F, java.lang.Double](starMagnitude, v)

      private val windSpeed = cs.map(_.getDouble("windspeed"))
      override def setWindSpeed(v: Double): F[Unit] = setParameter[F, java.lang.Double](windSpeed, v)
    }

  override val aoFlatten: EpicsCommand[F] = new EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoFlatten"))
  }

  override val aoStatistics: AoStatistics[F] = new EpicsCommandBase[F] with AoStatistics[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoStats"))

    private val fileName = cs.map(_.getString("filename"))
    override def setFileName(v: String): F[Unit] = setParameter(fileName, v)

    private val samples = cs.map(_.getInteger("samples"))
    override def setSamples(v: Int): F[Unit] = setParameter[F, java.lang.Integer](samples, v)

    private val interval = cs.map(_.getDouble("interval"))
    override def setInterval(v: Double): F[Unit] = setParameter[F, java.lang.Double](interval, v)

    private val triggerTime = cs.map(_.getDouble("trigtime"))
    override def setTriggerTimeInterval(v: Double): F[Unit] = setParameter[F, java.lang.Double](triggerTime, v)
  }

  override val targetFilter: TargetFilter[F] = new EpicsCommandBase[F] with TargetFilter[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("filter1"))

    private val bandwidth = cs.map(_.getDouble("bandwidth"))
    override def setBandwidth(v: Double): F[Unit] = setParameter[F, java.lang.Double](bandwidth, v)

    private val maxVelocity = cs.map(_.getDouble("maxv"))
    override def setMaxVelocity(v: Double): F[Unit] = setParameter[F, java.lang.Double](maxVelocity, v)

    private val grabRadius = cs.map(_.getDouble("grab"))
    override def setGrabRadius(v: Double): F[Unit] = setParameter[F, java.lang.Double](grabRadius, v)

    private val shortCircuit = cs.map(_.getString("shortCircuit"))
    override def setShortCircuit(v: String): F[Unit] = setParameter(shortCircuit, v)
  }

  private val tcsState = epicsService.getStatusAcceptor("tcsstate")

  override def absorbTipTilt: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("absorbTipTilt"))

  override def m1GuideSource: F[String] = safeAttributeF(tcsState.getStringAttribute("m1GuideSource"))

  private val m1GuideAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m1Guide",
    s"${TcsTop}im:m1GuideOn", classOf[BinaryOnOff], "M1 guide")
  override def m1Guide: F[BinaryOnOff] = safeAttributeF(m1GuideAttr)

  override def m2p1Guide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2p1Guide"))

  override def m2p2Guide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2p2Guide"))

  override def m2oiGuide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2oiGuide"))

  override def m2aoGuide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2aoGuide"))

  override def comaCorrect: F[String] = safeAttributeF(tcsState.getStringAttribute("comaCorrect"))

  private val m2GuideStateAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m2GuideState",
    s"${TcsTop}om:m2GuideState", classOf[BinaryOnOff], "M2 guiding state")
  override def m2GuideState: F[BinaryOnOff] = safeAttributeF(m2GuideStateAttr)

  override def xoffsetPoA1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoA1"))

  override def yoffsetPoA1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoA1"))

  override def xoffsetPoB1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoB1"))

  override def yoffsetPoB1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoB1"))

  override def xoffsetPoC1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoC1"))

  override def yoffsetPoC1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoC1"))

  override def sourceAWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceAWavelength"))

  override def sourceBWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceBWavelength"))

  override def sourceCWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceCWavelength"))

  override def chopBeam: F[String] = safeAttributeF(tcsState.getStringAttribute("chopBeam"))

  override def p1FollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("p1FollowS"))

  override def p2FollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("p2FollowS"))

  override def oiFollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("oiFollowS"))

  override def aoFollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("aoFollowS"))

  override def p1Parked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("p1Parked"))
    .map(_ =!= 0)

  override def p2Parked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("p2Parked"))
    .map(_ =!= 0)

  override def oiParked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("oiParked"))
    .map(_ =!= 0)

  private val pwfs1OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs1On",
    s"${TcsTop}drives:p1Integrating", classOf[BinaryYesNo], "P1 integrating")
  override def pwfs1On: F[BinaryYesNo] = safeAttributeF(pwfs1OnAttr)

  private val pwfs2OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs2On",
    s"${TcsTop}drives:p2Integrating", classOf[BinaryYesNo], "P2 integrating")

  override def pwfs2On:F[BinaryYesNo] = safeAttributeF(pwfs2OnAttr)

  private val oiwfsOnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("oiwfsOn",
    s"${TcsTop}drives:oiIntegrating", classOf[BinaryYesNo], "P2 integrating")

  override def oiwfsOn: F[BinaryYesNo] = safeAttributeF(oiwfsOnAttr)

  override def sfName: F[String] = safeAttributeF(tcsState.getStringAttribute("sfName"))

  override def sfParked: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("sfParked"))

  override def agHwName: F[String] = safeAttributeF(tcsState.getStringAttribute("agHwName"))

  override def agHwParked: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("agHwParked"))

  override def instrAA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrAA"))

  private val inPositionAttr: CaAttribute[String] = tcsState.getStringAttribute("inPosition")

  override def inPosition:F[String] = safeAttributeF(inPositionAttr)

  private val agInPositionAttr: CaAttribute[java.lang.Double] = tcsState.getDoubleAttribute("agInPosition")
  override def agInPosition:F[Double] = safeAttributeSDoubleF(agInPositionAttr)

  override val pwfs1ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("p1", tcsState)

  override val pwfs2ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("p2", tcsState)

  override val oiwfsProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("oi", tcsState)

  private val defaultTcsStabilizeTime = Duration.ofSeconds(1)

  private val filteredInPositionAttr: CaWindowStabilizer[String] =
    new CaWindowStabilizer[String](inPositionAttr, defaultTcsStabilizeTime)

  // Tcs fudge1 (time to wait for in-position to change to false)
  private val tcsSettleTime = FiniteDuration(2800, MILLISECONDS)

  // This functions returns a F that, when run, first waits tcsSettleTime to absorb in-position transients, then waits
  // for the in-position to change to true and stay true for stabilizationTime. It will wait up to `timeout`
  // seconds for that to happen.
  override def waitInPosition(stabilizationTime: Duration, timeout: FiniteDuration)(implicit T: Timer[F]): F[Unit] =
    T.sleep(FiniteDuration(tcsSettleTime.toMillis, TimeUnit.MILLISECONDS)) *>
      Sync[F].delay(filteredInPositionAttr.restart(stabilizationTime))
        .flatMap(waitForValueF(_, "TRUE", timeout,"TCS inposition flag"))

  private val agStabilizeTime = Duration.ofSeconds(1)

  private val filteredAGInPositionAttr: CaWindowStabilizer[java.lang.Double] =
    new CaWindowStabilizer[java.lang.Double](agInPositionAttr, agStabilizeTime)

  def filteredAGInPosition: F[Double] = safeAttributeSDoubleF(filteredAGInPositionAttr)

  // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
  /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
   * an error. A better solution is to detect the edge, from not in position to in-position.
   */
  private val AGSettleTime = FiniteDuration(1100, MILLISECONDS)
  override def waitAGInPosition(timeout: FiniteDuration)(implicit T: Timer[F]): F[Unit] =
    T.sleep(AGSettleTime) *>
      Sync[F].delay(filteredAGInPositionAttr.restart).flatMap(
        waitForValueF[java.lang.Double, F](_, 1.0, timeout, "AG inposition flag"))

  override def hourAngle: F[String] = safeAttributeF(tcsState.getStringAttribute("ha"))

  override def localTime: F[String] = safeAttributeF(tcsState.getStringAttribute("lt"))

  override def trackingFrame: F[String] = safeAttributeF(tcsState.getStringAttribute("trkframe"))

  override def trackingEpoch: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("trkepoch"))

  override def equinox: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceAEquinox"))

  override def trackingEquinox: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceATrackEq"))

  override def trackingDec: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("dectrack"))

  override def trackingRA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("ratrack"))

  override def elevation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("elevatio"))

  override def azimuth: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("azimuth"))

  override def crPositionAngle: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("crpa"))

  override def ut: F[String] = safeAttributeF(tcsState.getStringAttribute("ut"))

  override def date: F[String] = safeAttributeF(tcsState.getStringAttribute("date"))

  override def m2Baffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2baffle"))

  override def m2CentralBaffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2cenbaff"))

  override def st: F[String] = safeAttributeF(tcsState.getStringAttribute("st"))

  override def sfRotation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sfrt2"))

  override def sfTilt: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sftilt"))

  override def sfLinear: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sflinear"))

  override def instrPA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrPA"))

  override def targetA: F[List[Double]] = safeAttributeSListSDoubleF(tcsState.getDoubleAttribute("targetA"))

  override def aoFoldPosition: F[String] = safeAttributeF(tcsState.getStringAttribute("aoName"))

  private val useAoAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("useAo",
    s"${TcsTop}im:AOConfigFlag.VAL", classOf[BinaryYesNo], "Using AO flag")
  override def useAo: F[BinaryYesNo] = safeAttributeF(useAoAttr)

  override def airmass: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("airmass"))

  override def airmassStart: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("amstart"))

  override def airmassEnd: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("amend"))

  override def carouselMode: F[String] = safeAttributeF(tcsState.getStringAttribute("cguidmod"))

  override def crFollow: F[Int]  = safeAttributeSIntF(tcsState.getIntegerAttribute("crfollow"))

  override def sourceATarget: Target[F] = new Target[F] {
    override def epoch: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceAEpoch"))

    override def equinox: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceAEquinox"))

    override def radialVelocity: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("radvel"))

    override def frame: F[String] = safeAttributeF(tcsState.getStringAttribute("frame"))

    override def centralWavelenght: F[Double] = sourceAWavelength

    override def ra: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("ra"))

    override def objectName: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceAObjectName"))

    override def dec: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("dec"))

    override def parallax: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("parallax"))

    override def properMotionRA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("pmra"))

    override def properMotionDec: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("pmdec"))
  }

  private def target(base: String): Target[F] = new Target[F] {
      override def epoch: F[String] = safeAttributeF(tcsState.getStringAttribute(base + "aepoch"))
      override def equinox: F[String] = safeAttributeF(tcsState.getStringAttribute(base + "aequin"))
      override def radialVelocity:F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "arv"))
      override def frame: F[String]  = safeAttributeF(tcsState.getStringAttribute(base + "aframe"))
      override def centralWavelenght:F[Double] =
        safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "awavel"))
      override def ra:F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "ara"))
      override def objectName: F[String] = safeAttributeF(tcsState.getStringAttribute(base + "aobjec"))
      override def dec:F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "adec"))
      override def parallax:F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "aparal"))
      override def properMotionRA:F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "apmra"))
      override def properMotionDec:F[Double] =
        safeAttributeSDoubleF(tcsState.getDoubleAttribute(base + "apmdec"))
    }

  override val pwfs1Target: Target[F] = target("p1")

  override val pwfs2Target: Target[F] = target("p2")

  override val oiwfsTarget: Target[F] = target("oi")

  override def parallacticAngle: F[Angle] =
    safeAttributeSDoubleF(tcsState.getDoubleAttribute("parangle")).map(Degrees(_))

  override def m2UserFocusOffset: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("m2ZUserOffset"))

  private val pwfs1Status = epicsService.getStatusAcceptor("pwfs1state")

  override def pwfs1IntegrationTime: F[Double] = safeAttributeSDoubleF(pwfs1Status.getDoubleAttribute("intTime"))

  private val pwfs2Status = epicsService.getStatusAcceptor("pwfs2state")

  override def pwfs2IntegrationTime: F[Double] = safeAttributeSDoubleF(pwfs2Status.getDoubleAttribute("intTime"))

  private val oiwfsStatus = epicsService.getStatusAcceptor("oiwfsstate")

  // Attribute must be changed back to Double after EPICS channel is fixed.
  override def oiwfsIntegrationTime:F[Double]  = safeAttributeSDoubleF(oiwfsStatus.getDoubleAttribute("intTime"))


  private def instPort(name: String): F[Int] =
    safeAttributeSIntF(tcsState.getIntegerAttribute(s"${name}Port"))

  override def gsaoiPort: F[Int] = instPort("gsaoi")
  override def gpiPort: F[Int]= instPort("gpi")
  override def f2Port: F[Int] = instPort("f2")
  override def niriPort: F[Int] = instPort("niri")
  override def gnirsPort: F[Int] = instPort("nirs")
  override def nifsPort: F[Int] = instPort("nifs")
  override def gmosPort: F[Int] = instPort("gmos")
  override def ghostPort: F[Int] = instPort("ghost")

  override def aoGuideStarX: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("aogsx"))

  override def aoGuideStarY: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("aogsy"))

  override def aoPreparedCMX: F[Double] = safeAttributeF(tcsState.getStringAttribute("cmprepx"))
    .map(_.toDouble)
    .adaptError{case e => SeqexecException(e)}

  override def aoPreparedCMY: F[Double] = safeAttributeF(tcsState.getStringAttribute("cmprepy"))
    .map(_.toDouble)
    .adaptError{case e => SeqexecException(e)}

  override val g1ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("g1Guide", epicsService)

  override val g2ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("g2Guide", epicsService)

  override val g3ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("g3Guide", epicsService)

  override val g4ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmdImpl("g4Guide", epicsService)

  override val wavelG1: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelG1", epicsService)

  override val wavelG2: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelG2", epicsService)

  override val wavelG3: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelG3", epicsService)

  override val wavelG4: TargetWavelengthCmd[F] = new TargetWavelengthCmdImpl[F]("wavelG4", epicsService)

  override def gwfs1Target: Target[F] = target("g1")

  override def gwfs2Target: Target[F] = target("g2")

  override def gwfs3Target: Target[F] = target("g3")

  override def gwfs4Target: Target[F] = target("g4")

  override val cwfs1ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("ngsPr1Follow", epicsService)

  override val cwfs2ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("ngsPr2Follow", epicsService)

  override val cwfs3ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("ngsPr3Follow", epicsService)

  override val odgw1FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("odgw1Follow", epicsService)

  override val odgw2FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("odgw2Follow", epicsService)

  override val odgw3FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("odgw3Follow", epicsService)

  override val odgw4FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmdImpl("odgw4Follow", epicsService)

  override val odgw1ParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw1Parked"))
  }

  override val odgw2ParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw2Parked"))
  }

  override val odgw3ParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw3Parked"))
  }

  override val odgw4ParkCmd: EpicsCommand[F] = new EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw4Parked"))
  }

  // GeMS statuses

  val cwfs1FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs1Follow",
    s"${TcsTop}ngsPr1FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def cwfs1Follow: F[Boolean] =
    safeAttributeF(cwfs1FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val cwfs2FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs2Follow",
    s"${TcsTop}ngsPr2FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def cwfs2Follow: F[Boolean] =
    safeAttributeF(cwfs2FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val cwfs3FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs3Follow",
    s"${TcsTop}ngsPr3FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def cwfs3Follow: F[Boolean] =
    safeAttributeF(cwfs3FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw1FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw1Follow",
    s"${TcsTop}odgw1FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def odgw1Follow: F[Boolean] =
    safeAttributeF(odgw1FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw2FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw2Follow",
    s"${TcsTop}odgw2FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def odgw2Follow: F[Boolean] =
    safeAttributeF(odgw2FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw3FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw3Follow",
    s"${TcsTop}odgw3FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def odgw3Follow: F[Boolean] =
    safeAttributeF(odgw3FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw4FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw4Follow",
    s"${TcsTop}odgw4FollowStat.VAL", classOf[BinaryEnabledDisabled])
  override def odgw4Follow: F[Boolean] =
    safeAttributeF(odgw4FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val OdgwParkedState: String = "Parked"

  override def odgw1Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw1Parked")).map(_ === OdgwParkedState)

  override def odgw2Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw2Parked")).map(_ === OdgwParkedState)

  override def odgw3Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw3Parked")).map(_ === OdgwParkedState)

  override def odgw4Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw4Parked")).map(_ === OdgwParkedState)

  override def g1MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g1MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g2MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g2MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g3MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g3MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g4MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g4MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  override def g1Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g1Wavelength"))

  override def g2Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g2Wavelength"))

  override def g3Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g3Wavelength"))

  override def g4Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g4Wavelength"))

  override val g1GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("g1", tcsState)

  override val g2GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("g2", tcsState)

  override val g3GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("g3", tcsState)

  override val g4GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfigImpl("g4", tcsState)

}

object TcsEpics extends EpicsSystem[TcsEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Tcs.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[TcsEpics[IO]] =
    Sync[F].delay(new TcsEpicsImpl[IO](service, tops))

  trait ProbeGuideCmd[F[_]] extends EpicsCommand[F] {
    def setNodachopa(v: String): F[Unit]
    def setNodachopb(v: String): F[Unit]
    def setNodbchopa(v: String): F[Unit]
    def setNodbchopb(v: String): F[Unit]
  }

  final class ProbeGuideCmdImpl[F[_]: Async](csName: String, epicsService: CaService)
    extends EpicsCommandBase[F] with ProbeGuideCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val nodachopa = cs.map(_.getString("nodachopa"))
    override def setNodachopa(v: String): F[Unit] = setParameter(nodachopa, v)

    private val nodachopb = cs.map(_.getString("nodachopb"))
    override def setNodachopb(v: String): F[Unit] = setParameter(nodachopb, v)

    private val nodbchopa = cs.map(_.getString("nodbchopa"))
    override def setNodbchopa(v: String): F[Unit] = setParameter(nodbchopa, v)

    private val nodbchopb = cs.map(_.getString("nodbchopb"))
    override def setNodbchopb(v: String): F[Unit] = setParameter(nodbchopb, v)
  }

  trait WfsObserveCmd[F[_]] extends EpicsCommand[F] {
    def setNoexp(v: Integer): F[Unit]
    def setInt(v: Double): F[Unit]
    def setOutopt(v: String): F[Unit]
    def setLabel(v: String): F[Unit]
    def setOutput(v: String): F[Unit]
    def setPath(v: String): F[Unit]
    def setName(v: String): F[Unit]
  }

  final class WfsObserveCmdImpl[F[_]: Async](csName: String, epicsService: CaService)
    extends EpicsCommandBase[F] with WfsObserveCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val noexp = cs.map(_.getInteger("noexp"))
    override def setNoexp(v: Integer): F[Unit] = setParameter(noexp, v)

    private val int = cs.map(_.getDouble("int"))
    override def setInt(v: Double): F[Unit] = setParameter[F, java.lang.Double](int, v)

    private val outopt = cs.map(_.getString("outopt"))
    override def setOutopt(v: String): F[Unit] = setParameter(outopt, v)

    private val label = cs.map(_.getString("label"))
    override def setLabel(v: String): F[Unit] = setParameter(label, v)

    private val output = cs.map(_.getString("output"))
    override def setOutput(v: String): F[Unit] = setParameter(output, v)

    private val path = cs.map(_.getString("path"))
    override def setPath(v: String): F[Unit] = setParameter(path, v)

    private val name = cs.map(_.getString("name"))
    override def setName(v: String): F[Unit] = setParameter(name, v)
  }

  trait ProbeFollowCmd[F[_]] extends EpicsCommand[F] {
    def setFollowState(v: String): F[Unit]
  }

  final class ProbeFollowCmdImpl[F[_]: Async](csName: String, epicsService: CaService)
    extends EpicsCommandBase[F] with ProbeFollowCmd[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val follow = cs.map(_.getString("followState"))
    override def setFollowState(v: String): F[Unit] = setParameter(follow, v)
  }

  trait TargetWavelengthCmd[F[_]] extends EpicsCommand[F] {
    def setWavel(v: Double): F[Unit]
  }

  final class TargetWavelengthCmdImpl[F[_]: Async](csName: String, epicsService: CaService)
    extends EpicsCommandBase[F] with TargetWavelengthCmd[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val wavel = cs.map(_.getDouble("wavel"))

    override def setWavel(v: Double): F[Unit] = setParameter[F, java.lang.Double](wavel, v)
  }

  trait ProbeGuideConfig[F[_]] {
    def nodachopa: F[Int]
    def nodachopb: F[Int]
    def nodbchopa: F[Int]
    def nodbchopb: F[Int]
  }

  final class ProbeGuideConfigImpl[F[_]: Sync](protected val prefix: String, protected val tcsState: CaStatusAcceptor)
    extends ProbeGuideConfig[F] {
    override def nodachopa: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute(prefix + "nodachopa"))
    override def nodachopb: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute(prefix + "nodachopb"))
    override def nodbchopa: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute(prefix + "nodbchopa"))
    override def nodbchopb: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute(prefix + "nodbchopb"))
  }

  trait Target[F[_]] {
    def objectName: F[String]
    def ra: F[Double]
    def dec: F[Double]
    def frame: F[String]
    def equinox: F[String]
    def epoch: F[String]
    def properMotionRA: F[Double]
    def properMotionDec: F[Double]
    def centralWavelenght: F[Double]
    def parallax: F[Double]
    def radialVelocity: F[Double]
  }

  // TODO: Delete me after fully moved to tagless
  implicit class TargetIOOps(val tio: Target[IO]) extends AnyVal{
    def to[F[_]: LiftIO]: Target[F] = new Target[F] {
      def objectName: F[String] = tio.objectName.to[F]
      def ra: F[Double] = tio.ra.to[F]
      def dec: F[Double] = tio.dec.to[F]
      def frame: F[String] = tio.frame.to[F]
      def equinox: F[String] = tio.equinox.to[F]
      def epoch: F[String] = tio.epoch.to[F]
      def properMotionRA: F[Double] = tio.properMotionRA.to[F]
      def properMotionDec: F[Double] = tio.properMotionDec.to[F]
      def centralWavelenght: F[Double] = tio.centralWavelenght.to[F]
      def parallax: F[Double] = tio.parallax.to[F]
      def radialVelocity: F[Double] = tio.radialVelocity.to[F]
    }
  }

  sealed trait VirtualGemsTelescope extends Product with Serializable
  object VirtualGemsTelescope {
    case object G1 extends VirtualGemsTelescope
    case object G2 extends VirtualGemsTelescope
    case object G3 extends VirtualGemsTelescope
    case object G4 extends VirtualGemsTelescope
  }

  trait M1GuideCmd[F[_]] extends EpicsCommand[F] {
    def setState(v: String): F[Unit]
  }

  trait M2GuideCmd[F[_]] extends EpicsCommand[F] {
    def setState(v: String): F[Unit]
  }

  trait M2GuideModeCmd[F[_]] extends EpicsCommand[F] {
    def setComa(v: String): F[Unit]
  }

  trait M2GuideConfigCmd[F[_]] extends EpicsCommand[F] {
    def setSource(v: String): F[Unit]
    def setBeam(v: String): F[Unit]
    def setReset(v: String): F[Unit]
  }

  trait MountGuideCmd[F[_]] extends EpicsCommand[F] {
    def setSource(v: String): F[Unit]
    def setP1Weight(v: Double): F[Unit]
    def setP2Weight(v: Double): F[Unit]
    def setMode(v: String): F[Unit]
  }

  trait OffsetCmd[F[_]] extends EpicsCommand[F] {
    def setX(v: Double): F[Unit]
    def setY(v: Double): F[Unit]
  }

  trait M2Beam[F[_]] extends EpicsCommand[F] {
    def setBeam(v: String): F[Unit]
  }

  trait HrwfsPosCmd[F[_]] extends EpicsCommand[F] {
    def setHrwfsPos(v: String): F[Unit]
  }

  trait ScienceFoldPosCmd[F[_]] extends EpicsCommand[F] {
    def setScfold(v: String): F[Unit]
  }

  trait AoCorrect[F[_]] extends EpicsCommand[F] {
    def setCorrections(v: String): F[Unit]
    def setGains(v: Int): F[Unit]
    def setMatrix(v: Int): F[Unit]
  }

  trait AoPrepareControlMatrix[F[_]] extends EpicsCommand[F] {
    def setX(v: Double): F[Unit]
    def setY(v: Double): F[Unit]
    def setSeeing(v: Double): F[Unit]
    def setStarMagnitude(v: Double): F[Unit]
    def setWindSpeed(v: Double): F[Unit]
  }

  trait AoStatistics[F[_]] extends EpicsCommand[F] {
    def setFileName(v: String): F[Unit]
    def setSamples(v: Int): F[Unit]
    def setInterval(v: Double): F[Unit]
    def setTriggerTimeInterval(v: Double): F[Unit]
  }

  trait TargetFilter[F[_]] extends EpicsCommand[F] {
    def setBandwidth(v: Double): F[Unit]
    def setMaxVelocity(v: Double): F[Unit]
    def setGrabRadius(v: Double): F[Unit]
    def setShortCircuit(v: String): F[Unit]
  }

}
