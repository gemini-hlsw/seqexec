// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.effect.IO
import cats.effect.Sync
import cats.effect.Async
import edu.gemini.epics.acm.{CaAttribute, CaCommandSender, CaService, CaStatusAcceptor}
import edu.gemini.seqexec.server.gcal.BinaryOnOff
import seqexec.server.EpicsSystem
import seqexec.server.EpicsCommand
import seqexec.server.EpicsCommandF
import seqexec.server.EpicsCommand.setParameterF
import seqexec.server.EpicsUtil.safeAttribute
import org.log4s.{Logger, getLogger}

/**
  * Created by jluhrs on 3/14/17.
  */
class GcalEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  import GcalEpics._

  val GcalTop: String = tops.get("gc").getOrElse("")

  def post: F[EpicsCommand.Result] = lampsCmd.post

  object shutterCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gcal::shutter"))

    private val position = cs.map(_.getString("position"))
    def setPosition(v: String): F[Unit] = setParameterF(position, v)
  }

  object filterCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gcal::filtSel"))

    private val name = cs.map(_.getString("name"))
    def setName(v: String): F[Unit] = setParameterF(name, v)
  }

  object diffuserCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gcal::diffuseSel"))

    private val name = cs.map(_.getString("name"))
    def setName(v: String): F[Unit] = setParameterF(name, v)
  }

  private def toLampState(v: BinaryOnOff): String = v.name

  object lampsCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gcal::lampSel"))

    private val nameAr = cs.map(_.getString("nameAr"))
    def setArLampName(v: String): F[Unit] = setParameterF(nameAr, v)

    private val stateAr = cs.map(_.getString("stateAr"))
    def setArLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateAr, v, toLampState)

    private val nameCuAr = cs.map(_.getString("nameCuAr"))
    def setCuArLampName(v: String): F[Unit] = setParameterF(nameCuAr, v)

    private val stateCuAr = cs.map(_.getString("stateCuAr"))
    def setCuArLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateCuAr, v, toLampState)

    private val nameIR = cs.map(_.getString("nameIR"))
    def setIRLampName(v: String): F[Unit] = setParameterF(nameIR, v)

    private val stateIR = cs.map(_.getString("stateIR"))
    def setIRLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateIR, v, toLampState)

    private val nameQH = cs.map(_.getString("nameQH"))
    def setQHLampName(v: String): F[Unit] = setParameterF(nameQH, v)

    private val stateQH = cs.map(_.getString("stateQH"))
    def setQHLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateQH, v, toLampState)

    private val nameXe = cs.map(_.getString("nameXe"))
    def setXeLampName(v: String): F[Unit] = setParameterF(nameXe, v)

    private val stateXe = cs.map(_.getString("stateXe"))
    def setXeLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateXe, v, toLampState)

    private val nameThAr = cs.map(_.getString("nameThAr"))
    def setThArLampName(v: String): F[Unit] = setParameterF(nameThAr, v)

    private val stateThAr = cs.map(_.getString("stateThAr"))
    def setThArLampOn(v: BinaryOnOff): F[Unit] = setParameterF(stateThAr, v, toLampState)
  }

  private val state = epicsService.getStatusAcceptor("gcal::status")

  def createLampAttribute(name: String, longName: String): EnumAttribute[BinaryOnOff] =
    new EnumAttribute[BinaryOnOff](state, name + "LampState", s"${GcalTop}${name}_LampState",
      s"${longName} lamp state")(classOf[BinaryOnOff])

  val lampAr: EnumAttribute[BinaryOnOff] = createLampAttribute("Ar", "Argon")

  val lampQH: EnumAttribute[BinaryOnOff] = createLampAttribute("QH", "Quartz Halogen")

  val lampCuAr: EnumAttribute[BinaryOnOff] = createLampAttribute("CuAr", "Copper Argon")

  val lampXe: EnumAttribute[BinaryOnOff] = createLampAttribute("Xe", "Xenon")

  val lampThAr: EnumAttribute[BinaryOnOff] = createLampAttribute("ThAr", "Thorium Argon")

  val lampIr: EnumAttribute[BinaryOnOff] = createLampAttribute("IR", "Infrared")

  def shutter: F[Option[String]] =
    safeAttribute(state.getStringAttribute("shutter"))

  def filter: F[Option[String]] =
    safeAttribute(state.getStringAttribute("filter"))

  def diffuser: F[Option[String]] =
    safeAttribute(state.getStringAttribute("diffuser"))
}

object GcalEpics extends EpicsSystem[GcalEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Gcal.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GcalEpics[IO](service, tops)

  class EnumAttribute[T <: Enum[T]](sa: CaStatusAcceptor, attrName: String, attrChannel: String, desc: String)(c: Class[T]) {
    private val attribute: Option[CaAttribute[T]] = Option(sa.addEnum(attrName, attrChannel, c, desc))
    def delay[F[_]: Sync]: F[Option[T]] = Sync[F].delay(attribute.flatMap(v => Option(v.value)))
  }

}
