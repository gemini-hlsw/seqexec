// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.altair.LgsSfoControl
import org.log4s.{Logger, getLogger}
import seqexec.server.{EpicsCommand, EpicsSystem, SeqAction}
import seqexec.server.EpicsCommand.setParameter
import cats.implicits._

class AltairEpics(service: CaService, tops: Map[String, String]) {
  val AltairTop: String = tops.getOrElse("ao", "ao:")

  object strapGateControl extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(service.getCommandSender("aoStrap"))

    val gate: Option[CaParameter[Integer]] = cs.map(_.getInteger("gate"))
    def setGate(v: Int): SeqAction[Unit] = setParameter(gate, Integer.valueOf(v))
  }

  object strapControl extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("strapCorrCtl"))

    val active: Option[CaParameter[Integer]] = cs.map(_.getInteger("onoff"))
    def setActive(v: Int): SeqAction[Unit] = setParameter(active, Integer.valueOf(v))
  }

  object sfoControl extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("aoSfoLoop"))

    val active: Option[CaParameter[LgsSfoControl]] = cs.map(_.addEnum[LgsSfoControl]("active",
      s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl], false))
    def setActive(v: LgsSfoControl): SeqAction[Unit] = setParameter(active, v)
  }

  object btoLoopControl extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("btoFsaLoopCtrl"))

    val active: Option[CaParameter[String]] = cs.map(_.getString("loop"))
    def setActive(v: String): SeqAction[Unit] = setParameter(active, v)
  }

  val status: CaStatusAcceptor = service.getStatusAcceptor("aostate")

  def strapTempStatus: Option[Boolean] = Option(status.getIntegerAttribute("strapTPStat").value)
    .map(_.toInt =!= 0)

  def strapGateLevel: Option[Int] = Option(status.getIntegerAttribute("strapgate").value)
    .map(_.toInt)

  def strapLoop: Option[Boolean] = Option(status.getIntegerAttribute("straploop").value)
    .map(_.toInt =!= 0)

  def strapRTStatus: Option[Boolean] = Option(status.getIntegerAttribute("strapRTStat").value)
    .map(_.toInt =!= 0)

  def strapHVStatus: Option[Boolean] = Option(status.getIntegerAttribute("strapHVStat").value)
    .map(_.toInt =!= 0)

  def sfoLoop: Option[LgsSfoControl] = Option(status.addEnum("sfoloop",
    s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl]).value)

  def aoLoop: Option[Boolean] = Option(status.getIntegerAttribute("aowfsOn").value)
    .map(_.toInt =!= 0)

  def aoSettled: Option[Boolean] = Option(status.getDoubleAttribute("straploop").value)
    .map(_.toDouble =!= 0.0)

  def matrixStartX: Option[Double] = Option(status.getDoubleAttribute("conmatx").value).map(_.toDouble)

  def matrixStartY: Option[Double] = Option(status.getDoubleAttribute("conmaty").value).map(_.toDouble)

  def controlMatrixCalc: Option[CarStateGEM5] = Option(status.addEnum[CarStateGEM5]("cmPrepBusy",
    s"${AltairTop}prepareCm.BUSY", classOf[CarStateGEM5]).value)

  def lgsP1: Option[Boolean] = Option(status.getIntegerAttribute("lgsp1On").value)
    .map(_.toInt =!= 0)

  def lgsOi: Option[Boolean] = Option(status.getIntegerAttribute("lgsoiOn").value)
    .map(_.toInt =!= 0)

  def aoFollow: Option[Boolean] = Option(status.getStringAttribute("aoFollowS").value)
    .map(_ === "ON")

}

object AltairEpics extends EpicsSystem[AltairEpics] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Altair.xml"

  override def build(service: CaService, tops: Map[String, String]) = new AltairEpics(service, tops)

}