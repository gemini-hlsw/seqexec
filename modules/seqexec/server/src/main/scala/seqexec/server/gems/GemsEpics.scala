// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.effect.{Async, IO, Sync}
import cats.implicits._
import edu.gemini.epics.acm.{CaCommandSender, CaService, CaStatusAcceptor}
import edu.gemini.seqexec.server.gems.{ApdState, LoopState, ReadyState}
import seqexec.server.{EpicsCommandBase, EpicsSystem, EpicsUtil}
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil.{safeAttributeF, safeAttributeSDoubleF, safeAttributeSListSFloatF, safeAttributeSListSIntF}
import seqexec.server.EpicsUtil.{safeAttributeSIntF, safeAttributeSListSDoubleF}

import scala.concurrent.duration.FiniteDuration

class GemsEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  private val MystTop = tops.getOrElse("myst", "myst:")
  private val RtcTop = tops.getOrElse("rtc", "rtc:")

  object LoopControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gems::seqLoopCtrl"))

    def setCommand(v: String): F[Unit] = setParameter(cs.map(_.getString("cmd")), v)

    def setReasons(v: String): F[Unit] = setParameter(cs.map(_.getString("reasons")), v)
  }

  object ApdControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gems::ttApdCtrl"))

    def setApd1Cmd(v: String): F[Unit] = setParameter(cs.map(_.getString("apd1Cmd")), v)

    def setApd2Cmd(v: String): F[Unit] = setParameter(cs.map(_.getString("apd2Cmd")), v)

    def setApd3Cmd(v: String): F[Unit] = setParameter(cs.map(_.getString("apd3Cmd")), v)
  }

  val mystStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::status")

  private val aniLoopAttr = mystStatus.addEnum("aniLoop", s"${MystTop}aniLoopStatus.VAL", classOf[LoopState],
    "ANI loop status")
  def aniLoop: F[LoopState] = safeAttributeF(aniLoopAttr)

  def astrometryReady: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("astromState"))
    .map(_ === ReadyState.Ready.toString)

  private val flexLoopAttr = mystStatus.addEnum("flexLoop", s"${MystTop}flexLoopStatus.VAL", classOf[LoopState],
    "Flexure loop status")
  def flexureLoop: F[LoopState] = safeAttributeF(flexLoopAttr)

  private val focusLoopAttr = mystStatus.addEnum("focLoop", s"${MystTop}focLoopStatus.VAL", classOf[LoopState],
    "Focus loop status")
  def focusLoop: F[LoopState] = safeAttributeF(focusLoopAttr)

  def lgsFlux: F[List[Float]] = safeAttributeSListSFloatF(mystStatus.getFloatAttribute("lgsFlux"))

  def lgsStrehl: F[Double] =  safeAttributeSDoubleF(mystStatus.getDoubleAttribute("lgsStrehl"))

  def rZero: F[Double] = safeAttributeSDoubleF(mystStatus.getDoubleAttribute("rZero"))

  def cnSquare: F[List[Double]] = safeAttributeSListSDoubleF(mystStatus.getDoubleAttribute("cnSquare"))

  def astroMode: F[String] = safeAttributeF(mystStatus.getStringAttribute("astroMode"))

  private val lgsLoopAttr = mystStatus.addEnum("lgsLoop", s"${MystTop}lgsLoopStatus.VAL", classOf[LoopState],
    "LGS loop status")
  def lgsLoop: F[LoopState] = safeAttributeF(lgsLoopAttr)

  private val lgsMatrixAttr = mystStatus.addEnum("lgsMatrix", s"${MystTop}lgsMatrixReady.VAL", classOf[ReadyState])
  def lgsMatrixReady: F[Boolean] = safeAttributeF(lgsMatrixAttr).map(_ === ReadyState.Ready)

  val usedStr: String = "Used"
  def cwfs1Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs1"))
    .map(_ === usedStr)

  def cwfs2Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs2"))
    .map(_ === usedStr)

  def cwfs3Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs3"))
    .map(_ === usedStr)

  def cwfs1Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("ngs1Mag"))
    .map(_.toDouble)

  def cwfs2Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("ngs2Mag"))
    .map(_.toDouble)

  def cwfs3Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("ngs3Mag"))
    .map(_.toDouble)

  def ngsFlux: F[List[Int]] = safeAttributeSListSIntF(mystStatus.getIntegerAttribute("ngsFlux"))

  def odgs1Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs1"))
    .map(_ === usedStr)

  def odgs2Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs2"))
    .map(_ === usedStr)

  def odgs3Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs3"))
    .map(_ === usedStr)

  def odgs4Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs4"))
    .map(_ === usedStr)

  def odgs1Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("odgs1Mag"))
    .map(_.toDouble)

  def odgs2Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("odgs2Mag"))
    .map(_.toDouble)

  def odgs3Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("odgs3Mag"))
    .map(_.toDouble)

  def odgs4Magnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("odgs4Mag"))
    .map(_.toDouble)

  def oigsUsed: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("oigs"))
    .map(_ === usedStr)

  def oigsMagnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("oigsMag"))
    .map(_.toDouble)

  private val scienceStateAttr = mystStatus.addEnum("sciReady", s"${MystTop}sciReady.VAL", classOf[ReadyState])
  def scienceReady: F[Boolean] = safeAttributeF(scienceStateAttr).map(_ === ReadyState.Ready)

  def waitForStableLoops(timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(scienceStateAttr, ReadyState.Ready, timeout, "GeMS science ready flag")

  private val ttLoopAttr = mystStatus.addEnum("ttLoop", s"${MystTop}ttLoopStatus.VAL", classOf[LoopState],
    "TT loop status")
  def ttLoop: F[LoopState] = safeAttributeF(ttLoopAttr)

  val rtcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::rtcsad")

  def lgsExpTime: F[Double] = safeAttributeSDoubleF(rtcStatus.getDoubleAttribute("lgsExp"))

  def ngsExpMult: F[Double] = safeAttributeSDoubleF(rtcStatus.getDoubleAttribute("ngsExpMult"))

  def sourceMask: F[Int] = safeAttributeSIntF(rtcStatus.getIntegerAttribute("sourceMask"))

  private val apd1Attr = rtcStatus.addEnum("tt1Active", s"${RtcTop}ngs:apd1.VAL", classOf[ApdState])
  def apd1Active: F[Boolean] = safeAttributeF(apd1Attr).map(_ === ApdState.ENABLED)

  private val apd2Attr = rtcStatus.addEnum("tt2Active", s"${RtcTop}ngs:apd2.VAL", classOf[ApdState])
  def apd2Active: F[Boolean] = safeAttributeF(apd2Attr).map(_ === ApdState.ENABLED)

  private val apd3Attr = rtcStatus.addEnum("tt3Active", s"${RtcTop}ngs:apd3.VAL", classOf[ApdState])
  def apd3Active: F[Boolean] = safeAttributeF(apd3Attr).map(_ === ApdState.ENABLED)

  val aomStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::aomsad")

  val closedStr = "CLOSED"
  def scienceAdcLoopActive: F[Boolean] = safeAttributeF(aomStatus.getStringAttribute("adcScLoop"))
    .map(_ === closedStr)

  def ngsAdcLoopActive: F[Boolean] = safeAttributeF(aomStatus.getStringAttribute("adcNgsLoop"))
    .map(_ === closedStr)

  def scienceAdcState: F[String] = safeAttributeF(aomStatus.getStringAttribute("adcScState"))

  def beamSplitterState: F[String] = safeAttributeF(aomStatus.getStringAttribute("beamSplitterState"))

}

object GemsEpics extends EpicsSystem[GemsEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gems.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GemsEpics[IO]] =
    Sync[F].delay(new GemsEpics[IO](service, tops))

}

