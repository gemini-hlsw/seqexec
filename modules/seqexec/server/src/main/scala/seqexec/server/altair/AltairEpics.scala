// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.{Async, IO, Sync}
import mouse.boolean._
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.altair.LgsSfoControl
import seqexec.server.{EpicsCommandBase, EpicsSystem, EpicsUtil}
import seqexec.server.EpicsCommandBase.setParameter
import cats.implicits._
import seqexec.server.EpicsUtil._

import scala.concurrent.duration.FiniteDuration

class AltairEpics[F[_]: Async](service: CaService, tops: Map[String, String]) {
  val AltairTop: String = tops.getOrElse("ao", "ao:")

  object strapGateControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(service.createTaskControlSender("aoStrap",
      s"${AltairTop}wfcs:strapGtCtl", "ALTAIR STRAP"))

    val gate: Option[CaParameter[Integer]] = cs.map(_.addInteger("gate", s"${AltairTop}wfcs:strapGtCtl.A",
      "Gate control", false))
    def setGate(v: Int): F[Unit] = setParameter(gate, Integer.valueOf(v))
  }

  object strapControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(service.createTaskControlSender("strapCorrCtl",
      s"${AltairTop}wfcs:strapCorrCtl", "ALTAIR SFO"))

    val active: Option[CaParameter[Integer]] = cs.map(_.addInteger("onoff", s"${AltairTop}wfcs:strapCorrCtl.A",
      "Strap onoff loop control", false))
    def setActive(v: Int): F[Unit] = setParameter(active, Integer.valueOf(v))
  }

  // sfoControl is a bit weird, in that changing the 'active' parameter takes effect immediately.
  object sfoControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("aoSfoLoop"))

    val active: Option[CaParameter[LgsSfoControl]] = cs.map(_.addEnum[LgsSfoControl]("active",
      s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl], false))
    def setActive(v: LgsSfoControl): F[Unit] = setParameter(active, v)
  }

  object btoLoopControl extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("btoFsaLoopCtrl"))

    val active: Option[CaParameter[String]] = cs.map(_.getString("loop"))
    def setActive(v: String): F[Unit] = setParameter(active, v)
  }

  val status: CaStatusAcceptor = service.getStatusAcceptor("aostate")

  def strapTempStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapTPStat")).map(_ =!= 0)

  private val strapGateAttr = status.getIntegerAttribute("strapgate")
  def strapGate: F[Int] = safeAttributeSIntF(strapGateAttr)

  def waitForStrapGate(v: Int, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(strapGateAttr, v:Integer, timeout, "Altair strap gate")

  private val strapLoopAttr = status.getIntegerAttribute("straploop")
  def strapLoop: F[Boolean] =
    safeAttributeSIntF(strapLoopAttr).map(_ =!= 0)

  def waitForStrapLoop(v: Boolean, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(strapLoopAttr, v.fold(1, 0):Integer, timeout, "Altair strap loop")

  def strapRTStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapRTStat")).map(_ =!= 0)

  def strapHVStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapHVStat")).map(_ =!= 0)

  private val sfoLoopAttr: CaAttribute[LgsSfoControl] = status.addEnum("sfoloop",
    s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl])
  def sfoLoop: F[LgsSfoControl] = safeAttributeF(sfoLoopAttr)

  def aoexpt: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("aoexpt"))

  def aocounts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aocounts"))

  def aoseeing: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("aoseeing"))

  def aowfsx: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsx"))

  def aowfsy: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsy"))

  def aowfsz: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsz"))

  def aogain: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aogain"))

  def aoncpa: F[String] = safeAttributeF(status.getStringAttribute("aoncpa"))

  def ngndfilt: F[String] = safeAttributeF(status.getStringAttribute("ngndfilt"))

  def astar: F[String] = safeAttributeF(status.getStringAttribute("astar"))

  def aoflex: F[String] = safeAttributeF(status.getStringAttribute("aoflex"))

  def lgustage: F[String] = safeAttributeF(status.getStringAttribute("lgustage"))

  def aobs: F[String] = safeAttributeF(status.getStringAttribute("aobs"))

  def aoLoop: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("aowfsOn"))
    .map(_ =!= 0)

  private val aoSettledAttr = status.getDoubleAttribute("aoSettled")
  def aoSettled: F[Boolean] = safeAttributeSDoubleF(aoSettledAttr)
    .map(_ =!= 0.0)

  def waitAoSettled(timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF[java.lang.Double, F](aoSettledAttr, 1.0, timeout, "AO settled flag")

  def matrixStartX: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("conmatx"))

  def matrixStartY: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("conmaty"))

  private val controlMatrixCalcAttr = status.addEnum[CarStateGEM5]("cmPrepBusy", s"${AltairTop}prepareCm.BUSY", classOf[CarStateGEM5])
  def controlMatrixCalc: F[CarStateGEM5] = safeAttributeF(controlMatrixCalcAttr)

  def waitMatrixCalc(v: CarStateGEM5, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(controlMatrixCalcAttr, v, timeout, "Atair control matrix calculation")

  def lgsP1: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("lgsp1On"))
    .map(_ =!= 0)

  def lgsOi: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("lgsoiOn"))
    .map(_ =!= 0)

  def aoFollow: F[Boolean] = safeAttributeF(status.getStringAttribute("aoFollowS"))
    .map(_ === "On")

  // Lgs channels
  def lgdfocus: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgdfocus"))

  def apd1: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd1"))

  def apd2: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd2"))

  def apd3: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd3"))

  def apd4: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd4"))

  def lgttexp: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("lgttexp"))

  def fsmtip: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("fsmtip"))

  def fsmtilt: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("fsmtilt"))

  def lgzmpos: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgzmpos"))

  def aozoom: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aozoom"))

  def aoza: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aoza"))

  def nathick: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("nathick"))

  def lgndfilt: F[String] = safeAttributeF(status.getStringAttribute("lgndfilt"))

  def lgttiris: F[String] = safeAttributeF(status.getStringAttribute("lgttiris"))

  val sfoStatus: CaStatusAcceptor = service.getStatusAcceptor("sfostate")

  def lgsfcnts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgsfcnts"))

  def lgsfexp: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgsfexp"))

}

object AltairEpics extends EpicsSystem[AltairEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Altair.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]):F[AltairEpics[IO]] =
    Sync[F].delay(new AltairEpics(service, tops))

}
