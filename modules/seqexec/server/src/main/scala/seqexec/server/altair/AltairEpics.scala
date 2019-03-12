// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.{IO, Async}
import mouse.boolean._
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.altair.LgsSfoControl
import org.log4s.{Logger, getLogger}
import seqexec.server.{EpicsCommandF, EpicsSystem, EpicsUtil}
import seqexec.server.EpicsCommand.setParameterF
import cats.implicits._
import seqexec.server.EpicsUtil._
import squants.Time

class AltairEpics[F[_]: Async](service: CaService, tops: Map[String, String]) {
  val AltairTop: String = tops.getOrElse("ao", "ao:")

  object strapGateControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(service.createTaskControlSender("aoStrap",
      s"${AltairTop}wfcs:strapGtCtl", "ALTAIR STRAP"))

    val gate: Option[CaParameter[Integer]] = cs.map(_.addInteger("gate", s"${AltairTop}wfcs:strapGtCtl.A",
      "Gate control", false))
    def setGate(v: Int): F[Unit] = setParameterF(gate, Integer.valueOf(v))
  }

  object strapControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(service.createTaskControlSender("strapCorrCtl",
      s"${AltairTop}wfcs:strapCorrCtl", "ALTAIR SFO"))

    val active: Option[CaParameter[Integer]] = cs.map(_.addInteger("onoff", s"${AltairTop}wfcs:strapCorrCtl.A",
      "Strap onoff loop control", false))
    def setActive(v: Int): F[Unit] = setParameterF(active, Integer.valueOf(v))
  }

  // sfoControl is a bit weird, in that changing the 'active' parameter takes effect immediately.
  object sfoControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("aoSfoLoop"))

    val active: Option[CaParameter[LgsSfoControl]] = cs.map(_.addEnum[LgsSfoControl]("active",
      s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl], false))
    def setActive(v: LgsSfoControl): F[Unit] = setParameterF(active, v)
  }

  object btoLoopControl extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("btoFsaLoopCtrl"))

    val active: Option[CaParameter[String]] = cs.map(_.getString("loop"))
    def setActive(v: String): F[Unit] = setParameterF(active, v)
  }

  val status: CaStatusAcceptor = service.getStatusAcceptor("aostate")

  def strapTempStatus: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("strapTPStat"))
    .map(_.map(_ =!= 0))

  private val strapGateAttr = status.getIntegerAttribute("strapgate")
  def strapGate: F[Option[Int]] = safeAttributeSInt(strapGateAttr)

  def waitForStrapGate(v: Int, timeout: Time): F[Unit] =
    EpicsUtil.waitForValueF(strapGateAttr, v:Integer, timeout, "Altair strap gate")

  private val strapLoopAttr = status.getIntegerAttribute("straploop")
  def strapLoop: F[Option[Boolean]] = safeAttributeSInt(strapLoopAttr).map(_.map(_ =!= 0))

  def waitForStrapLoop(v: Boolean, timeout: Time): F[Unit] =
    EpicsUtil.waitForValueF(strapLoopAttr, v.fold(1, 0):Integer, timeout, "Altair strap loop")

  def strapRTStatus: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("strapRTStat"))
    .map(_.map(_ =!= 0))

  def strapHVStatus: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("strapHVStat"))
    .map(_.map(_ =!= 0))

  private val sfoLoopAttr: CaAttribute[LgsSfoControl] = status.addEnum("sfoloop",
    s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl])
  def sfoLoop: F[Option[LgsSfoControl]] = safeAttribute(sfoLoopAttr)

  def aoLoop: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("aowfsOn"))
    .map(_.map(_ =!= 0))

  private val aoSettledAttr = status.getDoubleAttribute("aoSettled")
  def aoSettled: F[Option[Boolean]] = safeAttributeSDouble(aoSettledAttr)
    .map(_.map(_ =!= 0.0))

  def waitAoSettled(timeout: Time): F[Unit] =
    EpicsUtil.waitForValueF[java.lang.Double, F](aoSettledAttr, 1.0, timeout, "AO settled flag")

  def matrixStartX: F[Option[Double]] = safeAttributeSDouble(status.getDoubleAttribute("conmatx"))

  def matrixStartY: F[Option[Double]] = safeAttributeSDouble(status.getDoubleAttribute("conmaty"))

  private val controlMatrixCalcAttr = status.addEnum[CarStateGEM5]("cmPrepBusy", s"${AltairTop}prepareCm.BUSY", classOf[CarStateGEM5])
  def controlMatrixCalc: F[Option[CarStateGEM5]] = safeAttribute(controlMatrixCalcAttr)

  def waitMatrixCalc(v: CarStateGEM5, timeout: Time): F[Unit] =
    EpicsUtil.waitForValueF(controlMatrixCalcAttr, v, timeout, "Atair control matrix calculation")

  def lgsP1: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("lgsp1On"))
    .map(_.map(_ =!= 0))

  def lgsOi: F[Option[Boolean]] = safeAttributeSInt(status.getIntegerAttribute("lgsoiOn"))
    .map(_.map(_ =!= 0))

  def aoFollow: F[Option[Boolean]] = safeAttribute(status.getStringAttribute("aoFollowS"))
    .map(_.map(_ === "On"))

}

object AltairEpics extends EpicsSystem[AltairEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Altair.xml"

  override def build(service: CaService, tops: Map[String, String]) = new AltairEpics(service, tops)

}