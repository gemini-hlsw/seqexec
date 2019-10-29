// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.{Async, IO, LiftIO, Sync}
import cats.implicits._
import squants.Angle
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.tcs.{BinaryEnabledDisabled, BinaryOnOff, BinaryYesNo}
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.EpicsCommand._
import seqexec.server.EpicsUtil._
import seqexec.server.SeqexecFailure.SeqexecException
import seqexec.server.{EpicsCommand, EpicsSystem}
import squants.Time
import squants.space.Degrees
import squants.time.TimeConversions._

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all the objects used
 * to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

final class TcsEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  import TcsEpics._

  val TcsTop: String = tops.getOrElse("tcs", "")

  // This is a bit ugly. Commands are triggered from the main apply record, so I just choose an arbitrary command here.
  // Triggering that command will trigger all the marked commands.
  def post: F[ApplyCommandResult] = m1GuideCmd.post

  object m1GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m1Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): F[Unit] = setParameter(state, v)
  }

  object m2GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): F[Unit] = setParameter(state, v)
  }

  object m2GuideModeCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideMode"))

    private val coma = cs.map(_.getString("coma"))
    def setComa(v: String): F[Unit] = setParameter(coma, v)
  }

  object m2GuideConfigCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2GuideConfig"))

    private val source = cs.map(_.getString("source"))
    def setSource(v: String): F[Unit] = setParameter(source, v)

    private val beam = cs.map(_.getString("beam"))
    def setBeam(v: String): F[Unit] = setParameter(beam, v)

    private val reset = cs.map(_.getString("reset"))
    def setReset(v: String): F[Unit] = setParameter(reset, v)
  }

  object mountGuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("mountGuide"))

    private val source = cs.map(_.getString("source"))

    def setSource(v: String): F[Unit] = setParameter(source, v)

    private val p1weight = cs.map(_.getDouble("p1weight"))

    def setP1Weight(v: Double): F[Unit] = setParameter[F, java.lang.Double](p1weight, v)

    private val p2weight = cs.map(_.getDouble("p2weight"))

    def setP2Weight(v: Double): F[Unit] = setParameter[F, java.lang.Double](p2weight, v)

    private val mode = cs.map(_.getString("mode"))

    def setMode(v: String): F[Unit] = setParameter(mode, v)
  }

  object offsetACmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoA1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)
  }

  object offsetBCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoB1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)
  }

  object offsetCCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoC1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)
  }

  val wavelSourceA: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelSourceA", epicsService)

  val wavelSourceB: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelSourceB", epicsService)

  val wavelSourceC: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelSourceC", epicsService)

  object m2Beam extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Beam"))

    private val beam = cs.map(_.getString("beam"))

    def setBeam(v: String): F[Unit] = setParameter(beam, v)
  }

  val pwfs1ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("pwfs1Guide", epicsService)

  val pwfs2ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("pwfs2Guide", epicsService)

  val oiwfsProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("oiwfsGuide", epicsService)

  val pwfs1ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("p1Follow", epicsService)

  val pwfs2ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("p2Follow", epicsService)

  val oiwfsProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("oiFollow", epicsService)

  val aoProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("aoFollow", epicsService)

  object pwfs1Park extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1Park"))
  }

  object pwfs2Park extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2Park"))
  }

  object oiwfsPark extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsPark"))
  }

  object pwfs1StopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs1StopObserve"))
  }

  object pwfs2StopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("pwfs2StopObserve"))
  }

  object oiwfsStopObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("oiwfsStopObserve"))
  }

  val pwfs1ObserveCmd: WfsObserveCmd[F] = new WfsObserveCmd("pwfs1Observe", epicsService)

  val pwfs2ObserveCmd: WfsObserveCmd[F] = new WfsObserveCmd("pwfs2Observe", epicsService)

  val oiwfsObserveCmd: WfsObserveCmd[F] = new WfsObserveCmd("oiwfsObserve", epicsService)

  object hrwfsParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfsPark"))
  }

  object hrwfsPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfs"))

    private val hrwfsPos = cs.map(_.getString("hrwfsPos"))

    def setHrwfsPos(v: String): F[Unit] = setParameter(hrwfsPos, v)
  }

  object scienceFoldParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFoldPark"))
  }

  object scienceFoldPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFold"))

    private val scfold = cs.map(_.getString("scfold"))

    def setScfold(v: String): F[Unit] = setParameter(scfold, v)
  }

  object observe extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::observe"))
  }

  object endObserve extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::endObserve"))
  }

  object aoCorrect extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoCorrect"))

    private val correct = cs.map(_.getString("correct"))
    def setCorrections(v: String): F[Unit] = setParameter(correct, v)

    private val gains = cs.map(_.getInteger("gains"))
    def setGains(v: Int): F[Unit] = setParameter[F, java.lang.Integer](gains, v)

    private val matrix = cs.map(_.getInteger("matrix"))
    def setMatrix(v: Int): F[Unit] = setParameter[F, java.lang.Integer](matrix, v)
  }

  object aoPrepareControlMatrix extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoPrepareCm"))

    private val x = cs.map(_.getDouble("x"))
    def setX(v: Double): F[Unit] = setParameter[F, java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))
    def setY(v: Double): F[Unit] = setParameter[F, java.lang.Double](y, v)

    private val seeing = cs.map(_.getDouble("seeing"))
    def setSeeing(v: Double): F[Unit] = setParameter[F, java.lang.Double](seeing, v)

    private val starMagnitude = cs.map(_.getDouble("gsmag"))
    def setStarMagnitude(v: Double): F[Unit] = setParameter[F, java.lang.Double](starMagnitude, v)

    private val windSpeed = cs.map(_.getDouble("windspeed"))
    def setWindSpeed(v: Double): F[Unit] = setParameter[F, java.lang.Double](windSpeed, v)
  }

  object aoFlatten extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoFlatten"))
  }

  object aoStatistics extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("aoStats"))

    private val fileName = cs.map(_.getString("filename"))
    def setFileName(v: String): F[Unit] = setParameter(fileName, v)

    private val samples = cs.map(_.getInteger("samples"))
    def setSamples(v: Int): F[Unit] = setParameter[F, java.lang.Integer](samples, v)

    private val interval = cs.map(_.getDouble("interval"))
    def setInterval(v: Double): F[Unit] = setParameter[F, java.lang.Double](interval, v)

    private val triggerTime = cs.map(_.getDouble("trigtime"))
    def setTriggerTimeInterval(v: Double): F[Unit] = setParameter[F, java.lang.Double](triggerTime, v)
  }

  object targetFilter extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("filter1"))

    private val bandwidth = cs.map(_.getDouble("bandwidth"))
    def setBandwidth(v: Double): F[Unit] = setParameter[F, java.lang.Double](bandwidth, v)

    private val maxVelocity = cs.map(_.getDouble("maxv"))
    def setMaxVelocity(v: Double): F[Unit] = setParameter[F, java.lang.Double](maxVelocity, v)

    private val grabRadius = cs.map(_.getDouble("grab"))
    def setGrabRadius(v: Double): F[Unit] = setParameter[F, java.lang.Double](grabRadius, v)

    private val shortCircuit = cs.map(_.getString("shortCircuit"))
    def setShortCircuit(v: String): F[Unit] = setParameter(shortCircuit, v)
  }

  private val tcsState = epicsService.getStatusAcceptor("tcsstate")

  def absorbTipTilt: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("absorbTipTilt"))

  def m1GuideSource: F[String] = safeAttributeF(tcsState.getStringAttribute("m1GuideSource"))

  private val m1GuideAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m1Guide",
    s"${TcsTop}im:m1GuideOn", classOf[BinaryOnOff], "M1 guide")
  def m1Guide: F[BinaryOnOff] = safeAttributeF(m1GuideAttr)

  def m2p1Guide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2p1Guide"))

  def m2p2Guide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2p2Guide"))

  def m2oiGuide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2oiGuide"))

  def m2aoGuide: F[String] = safeAttributeF(tcsState.getStringAttribute("m2aoGuide"))

  def comaCorrect: F[String] = safeAttributeF(tcsState.getStringAttribute("comaCorrect"))

  private val m2GuideStateAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m2GuideState",
    s"${TcsTop}om:m2GuideState", classOf[BinaryOnOff], "M2 guiding state")
  def m2GuideState: F[BinaryOnOff] = safeAttributeF(m2GuideStateAttr)

  def xoffsetPoA1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoA1"))

  def yoffsetPoA1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoA1"))

  def xoffsetPoB1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoB1"))

  def yoffsetPoB1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoB1"))

  def xoffsetPoC1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("xoffsetPoC1"))

  def yoffsetPoC1: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("yoffsetPoC1"))

  def sourceAWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceAWavelength"))

  def sourceBWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceBWavelength"))

  def sourceCWavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceCWavelength"))

  def chopBeam: F[String] = safeAttributeF(tcsState.getStringAttribute("chopBeam"))

  def p1FollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("p1FollowS"))

  def p2FollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("p2FollowS"))

  def oiFollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("oiFollowS"))

  def aoFollowS: F[String] = safeAttributeF(tcsState.getStringAttribute("aoFollowS"))

  def p1Parked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("p1Parked"))
    .map(_ =!= 0)

  def p2Parked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("p2Parked"))
    .map(_ =!= 0)

  def oiParked: F[Boolean] = safeAttributeSIntF(tcsState.getIntegerAttribute("oiParked"))
    .map(_ =!= 0)

  private val pwfs1OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs1On",
    s"${TcsTop}drives:p1Integrating", classOf[BinaryYesNo], "P1 integrating")
  def pwfs1On: F[BinaryYesNo] = safeAttributeF(pwfs1OnAttr)

  private val pwfs2OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs2On",
    s"${TcsTop}drives:p2Integrating", classOf[BinaryYesNo], "P2 integrating")

  def pwfs2On:F[BinaryYesNo] = safeAttributeF(pwfs2OnAttr)

  private val oiwfsOnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("oiwfsOn",
    s"${TcsTop}drives:oiIntegrating", classOf[BinaryYesNo], "P2 integrating")

  def oiwfsOn: F[BinaryYesNo] = safeAttributeF(oiwfsOnAttr)

  def sfName: F[String] = safeAttributeF(tcsState.getStringAttribute("sfName"))

  def sfParked: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("sfParked"))

  def agHwName: F[String] = safeAttributeF(tcsState.getStringAttribute("agHwName"))

  def agHwParked: F[Int] = safeAttributeSIntF(tcsState.getIntegerAttribute("agHwParked"))

  def instrAA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrAA"))

  private val inPositionAttr: CaAttribute[String] = tcsState.getStringAttribute("inPosition")

  def inPosition:F[String] = safeAttributeF(inPositionAttr)

  private val agInPositionAttr: CaAttribute[java.lang.Double] = tcsState.getDoubleAttribute("agInPosition")
  def agInPosition:F[Double] = safeAttributeSDoubleF(agInPositionAttr)

  val pwfs1ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("p1", tcsState)

  val pwfs2ProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("p2", tcsState)

  val oiwfsProbeGuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("oi", tcsState)

  private val tcsStabilizeTime = 1.seconds

  private val filteredInPositionAttr: CaWindowStabilizer[String] = new CaWindowStabilizer[String](inPositionAttr, java.time.Duration.ofMillis(tcsStabilizeTime.toMillis))
  def filteredInPosition:F[String] = safeAttributeF(filteredInPositionAttr)

  // This functions returns a SeqAction that, when run, will wait up to `timeout`
  // seconds for the TCS in-position flag to set to TRUE
  def waitInPosition(timeout: Time): F[Unit] = Sync[F].delay(filteredInPositionAttr.reset)
    .flatMap(waitForValueF(_, "TRUE", timeout,"TCS inposition flag"))

  private val agStabilizeTime = 1.seconds

  private val filteredAGInPositionAttr: CaWindowStabilizer[java.lang.Double] = new CaWindowStabilizer[java.lang.Double](agInPositionAttr, java.time.Duration.ofMillis(agStabilizeTime.toMillis))
  def filteredAGInPosition: F[Double] = safeAttributeSDoubleF(filteredAGInPositionAttr)

  // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
  /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
   * an error. A better solution is to detect the edge, from not in position to in-position.
   */
  private val AGSettleTime = 1100.milliseconds
  def waitAGInPosition(timeout: Time): F[Unit] = Sync[F].delay(Thread.sleep(AGSettleTime.toMilliseconds.toLong)) *>
    Sync[F].delay(filteredAGInPositionAttr.reset).flatMap(
      waitForValueF[java.lang.Double, F](_, 1.0, timeout, "AG inposition flag"))

  def hourAngle: F[String] = safeAttributeF(tcsState.getStringAttribute("ha"))

  def localTime: F[String] = safeAttributeF(tcsState.getStringAttribute("lt"))

  def trackingFrame: F[String] = safeAttributeF(tcsState.getStringAttribute("trkframe"))

  def trackingEpoch: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("trkepoch"))

  def equinox: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sourceAEquinox"))

  def trackingEquinox: F[String] = safeAttributeF(tcsState.getStringAttribute("sourceATrackEq"))

  def trackingDec: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("dectrack"))

  def trackingRA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("ratrack"))

  def elevation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("elevatio"))

  def azimuth: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("azimuth"))

  def crPositionAngle: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("crpa"))

  def ut: F[String] = safeAttributeF(tcsState.getStringAttribute("ut"))

  def date: F[String] = safeAttributeF(tcsState.getStringAttribute("date"))

  def m2Baffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2baffle"))

  def m2CentralBaffle: F[String] = safeAttributeF(tcsState.getStringAttribute("m2cenbaff"))

  def st: F[String] = safeAttributeF(tcsState.getStringAttribute("st"))

  def sfRotation: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sfrt2"))

  def sfTilt: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sftilt"))

  def sfLinear: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("sflinear"))

  def instrPA: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("instrPA"))

  def targetA: F[List[Double]] = safeAttributeSListSDoubleF(tcsState.getDoubleAttribute("targetA"))

  def aoFoldPosition: F[String] = safeAttributeF(tcsState.getStringAttribute("aoName"))

  private val useAoAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("useAo",
    s"${TcsTop}im:AOConfigFlag.VAL", classOf[BinaryYesNo], "Using AO flag")
  def useAo: F[BinaryYesNo] = safeAttributeF(useAoAttr)

  def airmass: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("airmass"))

  def airmassStart: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("amstart"))

  def airmassEnd: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("amend"))

  def carouselMode: F[String] = safeAttributeF(tcsState.getStringAttribute("cguidmod"))

  def crFollow: F[Int]  = safeAttributeSIntF(tcsState.getIntegerAttribute("crfollow"))

  def sourceATarget: Target[F] = new Target[F] {
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

  val pwfs1Target: Target[F] = target("p1")

  val pwfs2Target: Target[F] = target("p2")

  val oiwfsTarget: Target[F] = target("oi")

  def parallacticAngle: F[Angle] =
    safeAttributeSDoubleF(tcsState.getDoubleAttribute("parangle")).map(Degrees(_))

  def m2UserFocusOffset: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("m2ZUserOffset"))

  private val pwfs1Status = epicsService.getStatusAcceptor("pwfs1state")

  def pwfs1IntegrationTime: F[Double] = safeAttributeSDoubleF(pwfs1Status.getDoubleAttribute("intTime"))

  private val pwfs2Status = epicsService.getStatusAcceptor("pwfs2state")

  def pwfs2IntegrationTime: F[Double] = safeAttributeSDoubleF(pwfs2Status.getDoubleAttribute("intTime"))

  private val oiwfsStatus = epicsService.getStatusAcceptor("oiwfsstate")

  // Attribute must be changed back to Double after EPICS channel is fixed.
  def oiwfsIntegrationTime:F[Double]  = safeAttributeSDoubleF(oiwfsStatus.getDoubleAttribute("intTime"))


  private def instPort(name: String): F[Int] =
    safeAttributeSIntF(tcsState.getIntegerAttribute(s"${name}Port"))

  def gsaoiPort: F[Int] = instPort("gsaoi")
  def gpiPort: F[Int]= instPort("gpi")
  def f2Port: F[Int] = instPort("f2")
  def niriPort: F[Int] = instPort("niri")
  def gnirsPort: F[Int] = instPort("nirs")
  def nifsPort: F[Int] = instPort("nifs")
  def gmosPort: F[Int] = instPort("gmos")
  def ghostPort: F[Int] = instPort("ghost")

  def aoGuideStarX: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("aogsx"))

  def aoGuideStarY: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("aogsy"))

  def aoPreparedCMX: F[Double] = safeAttributeF(tcsState.getStringAttribute("cmprepx"))
    .map(_.toDouble)
    .adaptError{case e => SeqexecException(e)}

  def aoPreparedCMY: F[Double] = safeAttributeF(tcsState.getStringAttribute("cmprepy"))
    .map(_.toDouble)
    .adaptError{case e => SeqexecException(e)}

  // GeMS Commands
  import VirtualGemsTelescope._

  val g1ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("g1Guide", epicsService)

  val g2ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("g2Guide", epicsService)

  val g3ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("g3Guide", epicsService)

  val g4ProbeGuideCmd: ProbeGuideCmd[F] = new ProbeGuideCmd("g4Guide", epicsService)

  def gemsProbeGuideCmd(g: VirtualGemsTelescope): ProbeGuideCmd[F] = g match {
    case G1 => g1ProbeGuideCmd
    case G2 => g2ProbeGuideCmd
    case G3 => g3ProbeGuideCmd
    case G4 => g4ProbeGuideCmd
  }

  val wavelG1: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelG1", epicsService)

  val wavelG2: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelG2", epicsService)

  val wavelG3: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelG3", epicsService)

  val wavelG4: TargetWavelengthCmd[F] = new TargetWavelengthCmd[F]("wavelG4", epicsService)

  def gemsWavelengthCmd(g: VirtualGemsTelescope): TargetWavelengthCmd[F] = g match {
    case G1 => wavelG1
    case G2 => wavelG2
    case G3 => wavelG3
    case G4 => wavelG4
  }

  def gwfs1Target: Target[F] = target("g1")

  def gwfs2Target: Target[F] = target("g2")

  def gwfs3Target: Target[F] = target("g3")

  def gwfs4Target: Target[F] = target("g4")

  def gemsTarget(g: VirtualGemsTelescope): Target[F] = g match {
    case G1 => gwfs1Target
    case G2 => gwfs2Target
    case G3 => gwfs3Target
    case G4 => gwfs4Target
  }

  val cwfs1ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("ngsPr1Follow", epicsService)

  val cwfs2ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("ngsPr2Follow", epicsService)

  val cwfs3ProbeFollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("ngsPr3Follow", epicsService)

  val odgw1FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("odgw1Follow", epicsService)

  val odgw2FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("odgw2Follow", epicsService)

  val odgw3FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("odgw3Follow", epicsService)

  val odgw4FollowCmd: ProbeFollowCmd[F] = new ProbeFollowCmd("odgw4Follow", epicsService)

  object odgw1ParkCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw1Parked"))
  }

  object odgw2ParkCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw2Parked"))
  }

  object odgw3ParkCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw3Parked"))
  }

  object odgw4ParkCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("odgw4Parked"))
  }

  // GeMS statuses

  val cwfs1FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs1Follow",
    s"${TcsTop}ngsPr1FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def cwfs1Follow: F[Boolean] =
    safeAttributeF(cwfs1FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val cwfs2FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs2Follow",
    s"${TcsTop}ngsPr2FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def cwfs2Follow: F[Boolean] =
    safeAttributeF(cwfs2FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val cwfs3FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("ngs3Follow",
    s"${TcsTop}ngsPr3FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def cwfs3Follow: F[Boolean] =
    safeAttributeF(cwfs3FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw1FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw1Follow",
    s"${TcsTop}odgw1FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def odgw1Follow: F[Boolean] =
    safeAttributeF(odgw1FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw2FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw2Follow",
    s"${TcsTop}odgw2FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def odgw2Follow: F[Boolean] =
    safeAttributeF(odgw2FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw3FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw3Follow",
    s"${TcsTop}odgw3FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def odgw3Follow: F[Boolean] =
    safeAttributeF(odgw3FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val odgw4FollowAttr: CaAttribute[BinaryEnabledDisabled] = tcsState.addEnum("odgw4Follow",
    s"${TcsTop}odgw4FollowStat.VAL", classOf[BinaryEnabledDisabled])
  def odgw4Follow: F[Boolean] =
    safeAttributeF(odgw4FollowAttr).map(_ === BinaryEnabledDisabled.Enabled)

  val OdgwParkedState: String = "Parked"

  def odgw1Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw1Parked")).map(_ === OdgwParkedState)

  def odgw2Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw2Parked")).map(_ === OdgwParkedState)

  def odgw3Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw3Parked")).map(_ === OdgwParkedState)

  def odgw4Parked: F[Boolean] =
    safeAttributeF(tcsState.getStringAttribute("odgw4Parked")).map(_ === OdgwParkedState)

  def g1MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g1MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  def g2MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g2MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  def g3MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g3MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  def g4MapName: F[Option[GemsSource]] =
    safeAttributeF(tcsState.getStringAttribute("g4MapName"))
      .map(x => GemsSource.all.find(_.epicsVal === x))

  def g1Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g1Wavelength"))

  def g2Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g2Wavelength"))

  def g3Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g3Wavelength"))

  def g4Wavelength: F[Double] = safeAttributeSDoubleF(tcsState.getDoubleAttribute("g4Wavelength"))

  def gemsWavelength(g: VirtualGemsTelescope): F[Double] = g match {
    case G1 => g1Wavelength
    case G2 => g2Wavelength
    case G3 => g3Wavelength
    case G4 => g4Wavelength
  }

  val g1GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("g1", tcsState)

  val g2GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("g2", tcsState)

  val g3GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("g3", tcsState)

  val g4GuideConfig: ProbeGuideConfig[F] = new ProbeGuideConfig("g4", tcsState)

  def gemsGuideConfig(g: VirtualGemsTelescope): ProbeGuideConfig[F] = g match {
    case G1 => g1GuideConfig
    case G2 => g2GuideConfig
    case G3 => g3GuideConfig
    case G4 => g4GuideConfig
  }

}

object TcsEpics extends EpicsSystem[TcsEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Tcs.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[TcsEpics[IO]] =
    Sync[F].delay(new TcsEpics[IO](service, tops))

  sealed class ProbeGuideCmd[F[_]: Async](csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val nodachopa = cs.map(_.getString("nodachopa"))
    def setNodachopa(v: String): F[Unit] = setParameter(nodachopa, v)

    private val nodachopb = cs.map(_.getString("nodachopb"))
    def setNodachopb(v: String): F[Unit] = setParameter(nodachopb, v)

    private val nodachopc = cs.map(_.getString("nodachopc"))
    def setNodachopc(v: String): F[Unit] = setParameter(nodachopc, v)

    private val nodbchopa = cs.map(_.getString("nodbchopa"))
    def setNodbchopa(v: String): F[Unit] = setParameter(nodbchopa, v)

    private val nodbchopb = cs.map(_.getString("nodbchopb"))
    def setNodbchopb(v: String): F[Unit] = setParameter(nodbchopb, v)

    private val nodbchopc = cs.map(_.getString("nodbchopc"))
    def setNodbchopc(v: String): F[Unit] = setParameter(nodbchopc, v)

    private val nodcchopa = cs.map(_.getString("nodcchopa"))
    def setNodcchopa(v: String): F[Unit] = setParameter(nodcchopa, v)

    private val nodcchopb = cs.map(_.getString("nodcchopb"))
    def setNodcchopb(v: String): F[Unit] = setParameter(nodcchopb, v)

    private val nodcchopc = cs.map(_.getString("nodcchopc"))
    def setNodcchopc(v: String): F[Unit] = setParameter(nodcchopc, v)
  }

  sealed class WfsObserveCmd[F[_]: Async](csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val noexp = cs.map(_.getInteger("noexp"))
    def setNoexp(v: Integer): F[Unit] = setParameter(noexp, v)

    private val int = cs.map(_.getDouble("int"))
    def setInt(v: Double): F[Unit] = setParameter[F, java.lang.Double](int, v)

    private val outopt = cs.map(_.getString("outopt"))
    def setOutopt(v: String): F[Unit] = setParameter(outopt, v)

    private val label = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)

    private val output = cs.map(_.getString("output"))
    def setOutput(v: String): F[Unit] = setParameter(output, v)

    private val path = cs.map(_.getString("path"))
    def setPath(v: String): F[Unit] = setParameter(path, v)

    private val name = cs.map(_.getString("name"))
    def setName(v: String): F[Unit] = setParameter(name, v)
  }

  final class ProbeFollowCmd[F[_]: Async](csName: String, epicsService: CaService) extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val follow = cs.map(_.getString("followState"))
    def setFollowState(v: String): F[Unit] = setParameter(follow, v)
  }

  final class TargetWavelengthCmd[F[_]: Async](csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): F[Unit] = setParameter[F, java.lang.Double](wavel, v)
  }

  class ProbeGuideConfig[F[_]: Sync](protected val prefix: String, protected val tcsState: CaStatusAcceptor) {
    def nodachopa: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodachopa"))
    def nodachopb: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodachopb"))
    def nodachopc: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodachopc"))
    def nodbchopa: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodbchopa"))
    def nodbchopb: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodbchopb"))
    def nodbchopc: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodbchopc"))
    def nodcchopa: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodcchopa"))
    def nodcchopb: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodcchopb"))
    def nodcchopc: F[String] = safeAttributeF(tcsState.getStringAttribute(prefix + "nodcchopc"))
  }

  sealed trait Target[F[_]] {
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

}
