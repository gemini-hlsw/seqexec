// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.data.Nested
import cats.effect.{Async, IO}
import cats.implicits._
import edu.gemini.epics.acm.{CaCommandSender, CaService, CaStatusAcceptor}
import edu.gemini.seqexec.server.gems.{ApdState, BinaryOnOff, LoopState, ReadyState}
import org.log4s.{Logger, getLogger}
import seqexec.server.{EpicsCommandF, EpicsSystem}
import seqexec.server.EpicsCommand.setParameterF
import seqexec.server.EpicsUtil.{safeAttribute, safeAttributeSDouble, safeAttributeSFloat, safeAttributeSInt}

class GemsEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  private val MystTop = tops.getOrElse("myst", "myst:")
  private val RtcTop = tops.getOrElse("rtc", "rtc:")
  private val AomTop = tops.getOrElse("aom", "aom:")

  object LoopControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gems::seqLoopCtrl"))

    def setCommand(v: String): F[Unit] = setParameterF(cs.map(_.getString("cmd")), v)

    def setReasons(v: String): F[Unit] = setParameterF(cs.map(_.getString("reasons")), v)
  }

  object ApdControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gems::ttApdCtrl"))

    def setApd1Cmd(v: String): F[Unit] = setParameterF(cs.map(_.getString("apd1Cmd")), v)

    def setApd2Cmd(v: String): F[Unit] = setParameterF(cs.map(_.getString("apd2Cmd")), v)

    def setApd3Cmd(v: String): F[Unit] = setParameterF(cs.map(_.getString("apd3Cmd")), v)
  }

  val mystStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::status")

  private val aniLoopAttr = mystStatus.addEnum("aniLoop", s"${MystTop}aniLoopStatus.VAL", classOf[LoopState],
    "ANI loop status")
  def aniLoop: F[Option[LoopState]] = safeAttribute(aniLoopAttr)

  def astrometryReady: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("astromState")))
    .map(_ === ReadyState.Ready.toString).value

  private val flexLoopAttr = mystStatus.addEnum("flexLoop", s"${MystTop}flexLoopStatus.VAL", classOf[LoopState],
    "Flexure loop status")
  def flexureLoop: F[Option[LoopState]] = safeAttribute(flexLoopAttr)

  private val focusLoopAttr = mystStatus.addEnum("focLoop", s"${MystTop}focLoopStatus.VAL", classOf[LoopState],
    "Focus loop status")
  def focusLoop: F[Option[LoopState]] = safeAttribute(focusLoopAttr)

  def lgsFlux: F[Option[Double]] = Nested(safeAttributeSFloat(mystStatus.getFloatAttribute("lgsFlux")))
    .map(_.toDouble).value

  private val lgsLoopAttr = mystStatus.addEnum("lgsLoop", s"${MystTop}lgsLoopStatus.VAL", classOf[LoopState],
    "LGS loop status")
  def lgsLoop: F[Option[LoopState]] = safeAttribute(lgsLoopAttr)

  private val lgsMatrixAttr = mystStatus.addEnum("lgsMatrix", s"${MystTop}lgsMatrixReady.VAL", classOf[ReadyState])
  def lgsMatrixReady: F[Option[Boolean]] = Nested(safeAttribute(lgsMatrixAttr)).map(_ === ReadyState.Ready).value

  val usedStr: String = "Used"
  def ngs1Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs1")))
    .map(_ === usedStr).value

  def ngs2Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs2")))
    .map(_ === usedStr).value

  def ngs3Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs3")))
    .map(_ === usedStr).value

  def ngs1Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs1Mag")))
    .map(_.toDouble).value

  def ngs2Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs2Mag")))
    .map(_.toDouble).value

  def ngs3Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("ngs3Mag")))
    .map(_.toDouble).value

  def ngsFlux: F[Option[Int]] = safeAttributeSInt(mystStatus.getIntegerAttribute("ngsFlux"))

  def odgs1Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs1")))
    .map(_ === usedStr).value

  def odgs2Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs2")))
    .map(_ === usedStr).value

  def odgs3Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs3")))
    .map(_ === usedStr).value

  def odgs4Used: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs4")))
    .map(_ === usedStr).value

  def odgs1Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs1Mag")))
    .map(_.toDouble).value

  def odgs2Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs2Mag")))
    .map(_.toDouble).value

  def odgs3Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs3Mag")))
    .map(_.toDouble).value

  def odgs4Magnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("odgs4Mag")))
    .map(_.toDouble).value

  def oigsUsed: F[Option[Boolean]] = Nested(safeAttribute(mystStatus.getStringAttribute("oigs")))
    .map(_ === usedStr).value

  def oigsMagnitude: F[Option[Double]] = Nested(safeAttribute(mystStatus.getStringAttribute("oigsMag")))
    .map(_.toDouble).value

  private val scienceStateAttr = mystStatus.addEnum("sciReady", s"${MystTop}sciReady.VAL", classOf[ReadyState])
  def scienceReady: F[Option[Boolean]] = Nested(safeAttribute(scienceStateAttr)).map(_ === ReadyState.Ready).value

  private val ttLoopAttr = mystStatus.addEnum("ttLoop", s"${MystTop}ttLoopStatus.VAL", classOf[LoopState],
    "TT loop status")
  def ttLoop: F[Option[LoopState]] = safeAttribute(ttLoopAttr)

  val rtcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::rtcsad")

  def lgsExpTime: F[Option[Double]] = safeAttributeSDouble(rtcStatus.getDoubleAttribute("lgsExp"))

  def ngsExpMult: F[Option[Double]] = safeAttributeSDouble(rtcStatus.getDoubleAttribute("ngsExpMult"))

  def sourceMask: F[Option[Int]] = safeAttributeSInt(rtcStatus.getIntegerAttribute("sourceMask"))

  private val apd1Attr = rtcStatus.addEnum("tt1Active", s"${RtcTop}ngs:apd1.VAL", classOf[ApdState])
  def apd1Active: F[Option[Boolean]] = Nested(safeAttribute(apd1Attr)).map(_ === ApdState.ENABLED).value

  private val apd2Attr = rtcStatus.addEnum("tt2Active", s"${RtcTop}ngs:apd2.VAL", classOf[ApdState])
  def apd2Active: F[Option[Boolean]] = Nested(safeAttribute(apd2Attr)).map(_ === ApdState.ENABLED).value

  private val apd3Attr = rtcStatus.addEnum("tt3Active", s"${RtcTop}ngs:apd3.VAL", classOf[ApdState])
  def apd3Active: F[Option[Boolean]] = Nested(safeAttribute(apd3Attr)).map(_ === ApdState.ENABLED).value

  val aomStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::aomsad")

  val closedStr = "CLOSED"
  def scienceAdcLoopActive: F[Option[Boolean]] = Nested(safeAttribute(aomStatus.getStringAttribute("adcScLoop")))
    .map(_ === closedStr).value

  def scienceAdcState: F[Option[String]] = safeAttribute(aomStatus.getStringAttribute("adcScState"))

  def beamSplitterState: F[Option[String]] = safeAttribute(aomStatus.getStringAttribute("beamSplitterState"))

  private val ngs1FollowAttr = aomStatus.addEnum("ngs1Follow", s"${AomTop}ngsPr1followS.VAL", classOf[BinaryOnOff])
  def ngs1FollowActive: F[Option[Boolean]] = Nested(safeAttribute(ngs1FollowAttr)).map(_ === BinaryOnOff.On).value

  private val ngs2FollowAttr = aomStatus.addEnum("ngs2Follow", s"${AomTop}ngsPr2followS.VAL", classOf[BinaryOnOff])
  def ngs2FollowActive: F[Option[Boolean]] = Nested(safeAttribute(ngs2FollowAttr)).map(_ === BinaryOnOff.On).value

  private val ngs3FollowAttr = aomStatus.addEnum("ngs3Follow", s"${AomTop}ngsPr3followS.VAL", classOf[BinaryOnOff])
  def ngs3FollowActive: F[Option[Boolean]] = Nested(safeAttribute(ngs3FollowAttr)).map(_ === BinaryOnOff.On).value

}

object GemsEpics extends EpicsSystem[GemsEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Gems.xml"

  override def build(service: CaService, tops: Map[String, String]) =
    new GemsEpics[IO](service, tops)

}

