// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.{IO, Sync}
import cats.implicits._
import gem.math.Angle
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCommand._
import seqexec.server.EpicsUtil._
import seqexec.server.{EpicsCommand, EpicsSystem, SeqAction}

import squants.Time
import squants.time.TimeConversions._

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all the objects used
 * to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

// scalastyle:off
final class TcsEpics[F[_]: Sync](epicsService: CaService, tops: Map[String, String]) {

  import EpicsCommand.setParameter
  import TcsEpics._

  val TcsTop: String = tops.getOrElse("tcs", "")

  // This is a bit ugly. Commands are triggered from the main apply record, so I just choose an arbitrary command here.
  // Triggering that command will trigger all the marked commands.
  def post: SeqAction[EpicsCommand.Result] = m1GuideCmd.post

  object m1GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m1Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): SeqAction[Unit] = setParameter(state, v)
  }

  object m2GuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Guide"))
    private val state = cs.map(_.getString("state"))

    def setState(v: String): SeqAction[Unit] = setParameter[String](state, v)
  }

  object mountGuideCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("mountGuide"))

    private val source = cs.map(_.getString("source"))

    def setSource(v: String): SeqAction[Unit] = setParameter(source, v)

    private val p1weight = cs.map(_.getDouble("p1weight"))

    def setP1Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p1weight, v)

    private val p2weight = cs.map(_.getDouble("p2weight"))

    def setP2Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p2weight, v)

    private val mode = cs.map(_.getString("mode"))

    def setMode(v: String): SeqAction[Unit] = setParameter(mode, v)
  }

  object offsetACmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoA1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetBCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoB1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetCCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("offsetPoC1"))

    private val x = cs.map(_.getDouble("x"))

    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    private val y = cs.map(_.getDouble("y"))

    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object wavelSourceA extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceA"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceB extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceB"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceC extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("wavelSourceC"))

    private val wavel = cs.map(_.getDouble("wavel"))

    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object m2Beam extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("m2Beam"))

    private val beam = cs.map(_.getString("beam"))

    def setBeam(v: String): SeqAction[Unit] = setParameter(beam, v)
  }

  object pwfs1ProbeGuideCmd extends ProbeGuideCmd("pwfs1Guide", epicsService)

  object pwfs2ProbeGuideCmd extends ProbeGuideCmd("pwfs2Guide", epicsService)

  object oiwfsProbeGuideCmd extends ProbeGuideCmd("oiwfsGuide", epicsService)

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

  object pwfs1ObserveCmd extends WfsObserveCmd("pwfs1Observe", epicsService)

  object pwfs2ObserveCmd extends WfsObserveCmd("pwfs2Observe", epicsService)

  object oiwfsObserveCmd extends WfsObserveCmd("oiwfsObserve", epicsService)

  object hrwfsParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfsPark"))
  }

  object hrwfsPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("hrwfs"))

    private val hrwfsPos = cs.map(_.getString("hrwfsPos"))

    def setHrwfsPos(v: String): SeqAction[Unit] = setParameter(hrwfsPos, v)
  }

  object scienceFoldParkCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFoldPark"))
  }

  object scienceFoldPosCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("scienceFold"))

    private val scfold = cs.map(_.getString("scfold"))

    def setScfold(v: String): SeqAction[Unit] = setParameter(scfold, v)
  }

  object observe extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::observe"))
  }

  object endObserve extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("tcs::endObserve"))
  }

  private val tcsState = epicsService.getStatusAcceptor("tcsstate")

  def absorbTipTilt: F[Option[Int]] = safeAttributeSInt(tcsState.getIntegerAttribute("absorbTipTilt"))

  def m1GuideSource: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m1GuideSource"))

  private val m1GuideAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m1Guide",
    s"${TcsTop}im:m1GuideOn", classOf[BinaryOnOff], "M1 guide")
  def m1Guide: F[Option[BinaryOnOff]] = safeAttribute(m1GuideAttr)

  def m2p1Guide: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2p1Guide"))

  def m2p2Guide: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2p2Guide"))

  def m2oiGuide: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2oiGuide"))

  def m2aoGuide: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2aoGuide"))

  def comaCorrect: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("comaCorrect"))

  private val m2GuideStateAttr: CaAttribute[BinaryOnOff] = tcsState.addEnum("m2GuideState",
    s"${TcsTop}om:m2GuideState", classOf[BinaryOnOff], "M2 guiding state")
  def m2GuideState: F[Option[BinaryOnOff]] = safeAttribute(m2GuideStateAttr)

  def xoffsetPoA1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("xoffsetPoA1"))

  def yoffsetPoA1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("yoffsetPoA1"))

  def xoffsetPoB1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("xoffsetPoB1"))

  def yoffsetPoB1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("yoffsetPoB1"))

  def xoffsetPoC1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("xoffsetPoC1"))

  def yoffsetPoC1: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("yoffsetPoC1"))

  def sourceAWavelength: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sourceAWavelength"))

  def sourceBWavelength: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sourceBWavelength"))

  def sourceCWavelength: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sourceCWavelength"))

  def chopBeam: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("chopBeam"))

  def p1FollowS: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("p1FollowS"))

  def p2FollowS: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("p2FollowS"))

  def oiFollowS: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("oiFollowS"))

  private val pwfs1OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs1On",
    s"${TcsTop}drives:p1Integrating", classOf[BinaryYesNo], "P1 integrating")
  def pwfs1On: F[Option[BinaryYesNo]] = safeAttribute(pwfs1OnAttr)

  private val pwfs2OnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("pwfs2On",
    s"${TcsTop}drives:p2Integrating", classOf[BinaryYesNo], "P2 integrating")

  def pwfs2On:F[Option[BinaryYesNo]] = safeAttribute(pwfs2OnAttr)

  private val oiwfsOnAttr: CaAttribute[BinaryYesNo] = tcsState.addEnum("oiwfsOn",
    s"${TcsTop}drives:oiIntegrating", classOf[BinaryYesNo], "P2 integrating")

  def oiwfsOn: F[Option[BinaryYesNo]] = safeAttribute(oiwfsOnAttr)

  def sfName: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("sfName"))

  def sfParked: F[Option[Int]] = safeAttributeSInt(tcsState.getIntegerAttribute("sfParked"))

  def agHwName: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("agHwName"))

  def agHwParked: F[Option[Int]] = safeAttributeSInt(tcsState.getIntegerAttribute("agHwParked"))

  def instrAA: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("instrAA"))

  private val inPositionAttr: CaAttribute[String] = tcsState.getStringAttribute("inPosition")

  def inPosition:F[Option[String]] = safeAttribute(inPositionAttr)

  private val agInPositionAttr: CaAttribute[java.lang.Double] = tcsState.getDoubleAttribute("agInPosition")
  def agInPosition:F[Option[Double]] = safeAttributeSDouble(agInPositionAttr)

  object pwfs1ProbeGuideConfig extends ProbeGuideConfig("p1", tcsState)

  object pwfs2ProbeGuideConfig extends ProbeGuideConfig("p2", tcsState)

  object oiwfsProbeGuideConfig extends ProbeGuideConfig("oi", tcsState)

  private val tcsStabilizeTime = 1.seconds

  private val filteredInPositionAttr: CaAttribute[String] = new CaWindowStabilizer[String](inPositionAttr, java.time.Duration.ofMillis(tcsStabilizeTime.toMillis))
  def filteredInPosition:F[Option[String]] = safeAttribute(filteredInPositionAttr)

  // This functions returns a Task that, when run, will wait up to `timeout`
  // seconds for the TCS in-position flag to set to TRUE
  def waitInPosition(timeout: Time): SeqAction[Unit] = waitForValue(filteredInPositionAttr, "TRUE", timeout,
    "TCS inposition flag")

  private val agStabilizeTime = 1.seconds

  private val filteredAGInPositionAttr: CaAttribute[java.lang.Double] = new CaWindowStabilizer[java.lang.Double](agInPositionAttr, java.time.Duration.ofMillis(agStabilizeTime.toMillis))
  def filteredAGInPosition: F[Option[Double]] = safeAttributeSDouble(filteredAGInPositionAttr)

  // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
  /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
   * an error. A better solution is to detect the edge, from not in position to in-position.
   */
  private val AGSettleTime = 1100.milliseconds
  def waitAGInPosition(timeout: Time): SeqAction[Unit] = SeqAction(Thread.sleep(AGSettleTime.toMilliseconds.toLong)) *>
    waitForValue[java.lang.Double](filteredAGInPositionAttr, 1.0, timeout, "AG inposition flag")

  def hourAngle: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("ha"))

  def localTime: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("lt"))

  def trackingFrame: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("trkframe"))

  def trackingEpoch: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("trkepoch"))

  def equinox: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sourceAEquinox"))

  def trackingEquinox: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("sourceATrackEq"))

  def trackingDec: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("dectrack"))

  def trackingRA: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("ratrack"))

  def elevation: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("elevatio"))

  def azimuth: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("azimuth"))

  def crPositionAngle: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("crpa"))

  def ut: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("ut"))

  def date: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("date"))

  def m2Baffle: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2baffle"))

  def m2CentralBaffle: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("m2cenbaff"))

  def st: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("st"))

  def sfRotation: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sfrt2"))

  def sfTilt: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sftilt"))

  def sfLinear: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("sflinear"))

  def instrPA: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("instrPA"))

  def targetA: F[Option[List[Double]]] = safeAttributeSListSDouble(tcsState.getDoubleAttribute("targetA"))

  def aoFoldPosition: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("aoName"))

  def airmass: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("airmass"))

  def airmassStart: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("amstart"))

  def airmassEnd: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("amend"))

  def carouselMode: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("cguidmod"))

  def crFollow: F[Option[Int]]  = safeAttributeSInt(tcsState.getIntegerAttribute("crfollow"))

  def sourceATarget: Target[F] = new Target[F] {
    override def epoch: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("sourceAEpoch"))

    override def equinox: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("sourceAEquinox"))

    override def radialVelocity:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("radvel"))

    override def frame: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("frame"))

    override def centralWavelenght: F[Option[Double]] = sourceAWavelength

    override def ra: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("ra"))

    override def objectName: F[Option[String]] = safeAttribute(tcsState.getStringAttribute("sourceAObjectName"))

    override def dec: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("dec"))

    override def parallax: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("parallax"))

    override def properMotionRA: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("pmra"))

    override def properMotionDec: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("pmdec"))
  }

  private def target(base: String): Target[F] = new Target[F] {
      override def epoch: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(base + "aepoch"))
      override def equinox: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(base + "aequin"))
      override def radialVelocity:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute(base + "arv"))
      override def frame: F[Option[String]]  = safeAttribute(tcsState.getStringAttribute(base + "aframe"))
      override def centralWavelenght:F[Option[Double]] =
        safeAttributeSDouble(tcsState.getDoubleAttribute(base + "awavel"))
      override def ra:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute(base + "ara"))
      override def objectName: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(base + "aobjec"))
      override def dec:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute(base + "adec"))
      override def parallax:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute(base + "aparal"))
      override def properMotionRA:F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute(base + "apmra"))
      override def properMotionDec:F[Option[Double]] =
        safeAttributeSDouble(tcsState.getDoubleAttribute(base + "apmdec"))
    }

  def pwfs1Target: Target[F] = target("p1")

  def pwfs2Target: Target[F] = target("p2")

  def oiwfsTarget: Target[F] = target("oi")

  def gwfs1Target: Target[F] = target("g1")

  def gwfs2Target: Target[F] = target("g2")

  def gwfs3Target: Target[F] = target("g3")

  def gwfs4Target: Target[F] = target("g4")

  def parallacticAngle: F[Option[Angle]] =
    safeAttributeSDouble(tcsState.getDoubleAttribute("parangle")).map(_.map(Angle.fromDoubleDegrees))

  def m2UserFocusOffset: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("m2ZUserOffset"))

  private val pwfs1Status = epicsService.getStatusAcceptor("pwfs1state")

  def pwfs1IntegrationTime: F[Option[Double]] = safeAttributeSDouble(pwfs1Status.getDoubleAttribute("intTime"))

  private val pwfs2Status = epicsService.getStatusAcceptor("pwfs2state")

  def pwfs2IntegrationTime: F[Option[Double]] = safeAttributeSDouble(pwfs2Status.getDoubleAttribute("intTime"))

  private val oiwfsStatus = epicsService.getStatusAcceptor("oiwfsstate")

  // Attribute must be changed back to Double after EPICS channel is fixed.
  def oiwfsIntegrationTime:F[Option[Double]]  = safeAttributeSDouble(oiwfsStatus.getDoubleAttribute("intTime"))


  private def instPort(name: String): F[Option[Int]] =
    safeAttributeSInt(tcsState.getIntegerAttribute(s"${name}Port"))

  def gsaoiPort: F[Option[Int]] = instPort("gsaoi")
  def gpiPort: F[Option[Int]]= instPort("gpi")
  def f2Port: F[Option[Int]] = instPort("f2")
  def niriPort: F[Option[Int]] = instPort("niri")
  def gnirsPort: F[Option[Int]] = instPort("nirs")
  def nifsPort: F[Option[Int]] = instPort("nifs")
  def gmosPort: F[Option[Int]] = instPort("gmos")
  def ghostPort: F[Option[Int]] = instPort("ghost")

  def aoGuideStarX: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("aogsx"))

  def aoGuideStarY: F[Option[Double]] = safeAttributeSDouble(tcsState.getDoubleAttribute("aogsy"))

}

object TcsEpics extends EpicsSystem[TcsEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Tcs.xml"

  override def build(service: CaService, tops: Map[String, String]) = new TcsEpics(service, tops)

  sealed class ProbeGuideCmd(csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val nodachopa = cs.map(_.getString("nodachopa"))
    def setNodachopa(v: String): SeqAction[Unit] = setParameter(nodachopa, v)

    private val nodachopb = cs.map(_.getString("nodachopb"))
    def setNodachopb(v: String): SeqAction[Unit] = setParameter(nodachopb, v)

    private val nodachopc = cs.map(_.getString("nodachopc"))
    def setNodachopc(v: String): SeqAction[Unit] = setParameter(nodachopc, v)

    private val nodbchopa = cs.map(_.getString("nodbchopa"))
    def setNodbchopa(v: String): SeqAction[Unit] = setParameter(nodbchopa, v)

    private val nodbchopb = cs.map(_.getString("nodbchopb"))
    def setNodbchopb(v: String): SeqAction[Unit] = setParameter(nodbchopb, v)

    private val nodbchopc = cs.map(_.getString("nodbchopc"))
    def setNodbchopc(v: String): SeqAction[Unit] = setParameter(nodbchopc, v)

    private val nodcchopa = cs.map(_.getString("nodcchopa"))
    def setNodcchopa(v: String): SeqAction[Unit] = setParameter(nodcchopa, v)

    private val nodcchopb = cs.map(_.getString("nodcchopb"))
    def setNodcchopb(v: String): SeqAction[Unit] = setParameter(nodcchopb, v)

    private val nodcchopc = cs.map(_.getString("nodcchopc"))
    def setNodcchopc(v: String): SeqAction[Unit] = setParameter(nodcchopc, v)
  }

  sealed class WfsObserveCmd(csName: String, epicsService: CaService) extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender(csName))

    private val noexp = cs.map(_.getInteger("noexp"))
    def setNoexp(v: Integer): SeqAction[Unit] = setParameter(noexp, v)

    private val int = cs.map(_.getDouble("int"))
    def setInt(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](int, v)

    private val outopt = cs.map(_.getString("outopt"))
    def setOutopt(v: String): SeqAction[Unit] = setParameter(outopt, v)

    private val label = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)

    private val output = cs.map(_.getString("output"))
    def setOutput(v: String): SeqAction[Unit] = setParameter(output, v)

    private val path = cs.map(_.getString("path"))
    def setPath(v: String): SeqAction[Unit] = setParameter(path, v)

    private val name = cs.map(_.getString("name"))
    def setName(v: String): SeqAction[Unit] = setParameter(name, v)
  }

  class ProbeGuideConfig[F[_]: Sync](protected val prefix: String, protected val tcsState: CaStatusAcceptor) {
    def nodachopa: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodachopa"))
    def nodachopb: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodachopb"))
    def nodachopc: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodachopc"))
    def nodbchopa: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodbchopa"))
    def nodbchopb: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodbchopb"))
    def nodbchopc: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodbchopc"))
    def nodcchopa: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodcchopa"))
    def nodcchopb: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodcchopb"))
    def nodcchopc: F[Option[String]] = safeAttribute(tcsState.getStringAttribute(prefix+"nodcchopc"))
  }

  sealed trait Target[F[_]] {
    def objectName: F[Option[String]]
    def ra: F[Option[Double]]
    def dec: F[Option[Double]]
    def frame: F[Option[String]]
    def equinox: F[Option[String]]
    def epoch: F[Option[String]]
    def properMotionRA: F[Option[Double]]
    def properMotionDec: F[Option[Double]]
    def centralWavelenght: F[Option[Double]]
    def parallax: F[Option[Double]]
    def radialVelocity: F[Option[Double]]
  }

}
// scalastyle:on
