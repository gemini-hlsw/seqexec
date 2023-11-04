// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import scala.concurrent.duration.FiniteDuration
import cats.effect.Async
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.epics.acm.CaCommandSender
import edu.gemini.epics.acm.CaService
import edu.gemini.epics.acm.CaStatusAcceptor
import edu.gemini.seqexec.server.gems.LoopState
import edu.gemini.seqexec.server.gems.ReadyState
import seqexec.server.{ EpicsCommand, EpicsCommandBase, EpicsSystem, EpicsUtil }
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil._

trait GemsEpics[F[_]] {
  import seqexec.server.gems.GemsEpics.LoopControl

  val loopControl: LoopControl[F]

  def aniLoop: F[LoopState]

  def astrometryReady: F[Boolean]

  def flexureLoop: F[LoopState]

  def focusLoop: F[LoopState]

  def lgsFlux: F[List[Float]]

  def lgsStrehl: F[Double]

  def rZero: F[Double]

  def cnSquare: F[List[Float]]

  def astroMode: F[String]

  def lgsLoop: F[LoopState]

  def lgsMatrixReady: F[Boolean]

  def cwfs1Used: F[Boolean]

  def cwfs2Used: F[Boolean]

  def cwfs3Used: F[Boolean]

  def cwfs1Magnitude: F[Double]

  def cwfs2Magnitude: F[Double]

  def cwfs3Magnitude: F[Double]

  def ngsFlux: F[List[Int]]

  def odgs1Used: F[Boolean]

  def odgs2Used: F[Boolean]

  def odgs3Used: F[Boolean]

  def odgs4Used: F[Boolean]

  def odgs1Magnitude: F[Double]

  def odgs2Magnitude: F[Double]

  def odgs3Magnitude: F[Double]

  def odgs4Magnitude: F[Double]

  def oigsUsed: F[Boolean]

  def oigsMagnitude: F[Double]

  def scienceReady: F[Boolean]

  def waitForStableLoops(timeout: FiniteDuration): F[Unit]

  def ttLoop: F[LoopState]

  def lgsExpTime: F[Double]

  def ngsExpMult: F[Double]

  def sourceMask: F[Int]

  def apd1Active: F[Boolean]

  def apd2Active: F[Boolean]

  def apd3Active: F[Boolean]

  def scienceAdcLoopActive: F[Boolean]

  def ngsAdcLoopActive: F[Boolean]

  def scienceAdcState: F[String]

  def beamSplitterState: F[String]

}

object GemsEpics extends EpicsSystem[GemsEpics[IO]] {

  override val className: String      = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gems.xml"

  trait LoopControl[F[_]] extends EpicsCommand[F] {
    def setCommand(v: String): F[Unit]
    def setReasons(v: String): F[Unit]
  }

  class GemsEpicsImpl[F[_]: Async](epicsService: CaService, tops: Map[String, String])
      extends GemsEpics[F] {
    val sysName: String = "GeMS"

    private val MystTop = tops.getOrElse("myst", "myst:")
    private val Ngs2Top = tops.getOrElse("ngs2", "ngs2:")

    override val loopControl: LoopControl[F] = new EpicsCommandBase[F](sysName)
      with LoopControl[F] {
      override protected val cs: Option[CaCommandSender] = Option(
        epicsService.getCommandSender("gems::seqLoopCtrl")
      )

      override def setCommand(v: String): F[Unit] = setParameter(cs.map(_.getString("cmd")), v)

      override def setReasons(v: String): F[Unit] = setParameter(cs.map(_.getString("reasons")), v)
    }

    private val mystStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::status")

    private val aniLoopAttr            = mystStatus.addEnum("aniLoop",
                                                 s"${MystTop}aniLoopStatus.VAL",
                                                 classOf[LoopState],
                                                 "ANI loop status"
    )
    override def aniLoop: F[LoopState] = safeAttributeF(aniLoopAttr)

    override def astrometryReady: F[Boolean] =
      safeAttributeF(mystStatus.getStringAttribute("astromState"))
        .map(_ === ReadyState.Ready.toString)

    private val flexLoopAttr               = mystStatus.addEnum("flexLoop",
                                                  s"${MystTop}flexLoopStatus.VAL",
                                                  classOf[LoopState],
                                                  "Flexure loop status"
    )
    override def flexureLoop: F[LoopState] = safeAttributeF(flexLoopAttr)

    private val focusLoopAttr            = mystStatus.addEnum("focLoop",
                                                   s"${MystTop}focLoopStatus.VAL",
                                                   classOf[LoopState],
                                                   "Focus loop status"
    )
    override def focusLoop: F[LoopState] = safeAttributeF(focusLoopAttr)

    override def lgsFlux: F[List[Float]] = safeAttributeSListSFloatF(
      mystStatus.getFloatAttribute("lgsFlux")
    )

    override def lgsStrehl: F[Double] = safeAttributeSDoubleF(
      mystStatus.getDoubleAttribute("lgsStrehl")
    )

    override def rZero: F[Double] = safeAttributeSDoubleF(mystStatus.getDoubleAttribute("rZero"))

    override def cnSquare: F[List[Float]] = safeAttributeSListSFloatF(
      mystStatus.getFloatAttribute("cnSquare")
    )

    override def astroMode: F[String] = safeAttributeF(mystStatus.getStringAttribute("astroMode"))

    private val lgsLoopAttr            = mystStatus.addEnum("lgsLoop",
                                                 s"${MystTop}lgsLoopStatus.VAL",
                                                 classOf[LoopState],
                                                 "LGS loop status"
    )
    override def lgsLoop: F[LoopState] = safeAttributeF(lgsLoopAttr)

    private val lgsMatrixAttr               =
      mystStatus.addEnum("lgsMatrix", s"${MystTop}lgsMatrixReady.VAL", classOf[ReadyState])
    override def lgsMatrixReady: F[Boolean] =
      safeAttributeF(lgsMatrixAttr).map(_ === ReadyState.Ready)

    val usedStr: String                = "Used"
    override def cwfs1Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs1"))
      .map(_ === usedStr)

    override def cwfs2Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs2"))
      .map(_ === usedStr)

    override def cwfs3Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("ngs3"))
      .map(_ === usedStr)

    override def cwfs1Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("ngs1Mag"))
        .map(_.toDouble)

    override def cwfs2Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("ngs2Mag"))
        .map(_.toDouble)

    override def cwfs3Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("ngs3Mag"))
        .map(_.toDouble)

    override def ngsFlux: F[List[Int]] = safeAttributeSListSIntF(
      mystStatus.getIntegerAttribute("ngsFlux")
    )

    override def odgs1Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs1"))
      .map(_ === usedStr)

    override def odgs2Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs2"))
      .map(_ === usedStr)

    override def odgs3Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs3"))
      .map(_ === usedStr)

    override def odgs4Used: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("odgs4"))
      .map(_ === usedStr)

    override def odgs1Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("odgs1Mag"))
        .map(_.toDouble)

    override def odgs2Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("odgs2Mag"))
        .map(_.toDouble)

    override def odgs3Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("odgs3Mag"))
        .map(_.toDouble)

    override def odgs4Magnitude: F[Double] =
      safeAttributeF(mystStatus.getStringAttribute("odgs4Mag"))
        .map(_.toDouble)

    override def oigsUsed: F[Boolean] = safeAttributeF(mystStatus.getStringAttribute("oigs"))
      .map(_ === usedStr)

    override def oigsMagnitude: F[Double] = safeAttributeF(mystStatus.getStringAttribute("oigsMag"))
      .map(_.toDouble)

    private val scienceStateAttr          =
      mystStatus.addEnum("sciReady", s"${MystTop}sciReady.VAL", classOf[ReadyState])
    override def scienceReady: F[Boolean] =
      safeAttributeF(scienceStateAttr).map(_ === ReadyState.Ready)

    override def waitForStableLoops(timeout: FiniteDuration): F[Unit] =
      EpicsUtil.waitForValueF(scienceStateAttr,
                              ReadyState.Ready,
                              timeout,
                              "GeMS science ready flag"
      )

    private val ttLoopAttr            = mystStatus.addEnum("ttLoop",
                                                s"${MystTop}ttLoopStatus.VAL",
                                                classOf[LoopState],
                                                "TT loop status"
    )
    override def ttLoop: F[LoopState] = safeAttributeF(ttLoopAttr)

    private val rtcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::rtcsad")

    override def lgsExpTime: F[Double] = safeAttributeSDoubleF(
      rtcStatus.getDoubleAttribute("lgsExp")
    )

    override def ngsExpMult: F[Double] = safeAttributeSDoubleF(
      rtcStatus.getDoubleAttribute("ngsExpMult")
    )

    override def sourceMask: F[Int] = safeAttributeSIntF(
      rtcStatus.getIntegerAttribute("sourceMask")
    )

    private val apd1Attr                = rtcStatus.addInteger("tt1Active", s"${Ngs2Top}wfsConfig.D")
    override def apd1Active: F[Boolean] = safeAttributeF(apd1Attr).map(_ > 0)

    private val apd2Attr                = rtcStatus.addInteger("tt2Active", s"${Ngs2Top}wfsConfig.I")
    override def apd2Active: F[Boolean] = safeAttributeF(apd2Attr).map(_ > 0)

    private val apd3Attr                = rtcStatus.addInteger("tt3Active", s"${Ngs2Top}wfsConfig.O")
    override def apd3Active: F[Boolean] = safeAttributeF(apd3Attr).map(_ > 0)

    private val aomStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("gems::aomsad")

    val closedStr                                 = "CLOSED"
    override def scienceAdcLoopActive: F[Boolean] =
      safeAttributeF(aomStatus.getStringAttribute("adcScLoop"))
        .map(_ === closedStr)

    override def ngsAdcLoopActive: F[Boolean] =
      safeAttributeF(aomStatus.getStringAttribute("adcNgsLoop"))
        .map(_ === closedStr)

    override def scienceAdcState: F[String] = safeAttributeF(
      aomStatus.getStringAttribute("adcScState")
    )

    override def beamSplitterState: F[String] = safeAttributeF(
      aomStatus.getStringAttribute("beamSplitterState")
    )

  }

  def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GemsEpics[IO]] =
    Sync[F].delay(new GemsEpicsImpl[IO](service, tops))

}
