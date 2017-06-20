package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.epics.acm.{CaAttribute, CaService, CaStatusAcceptor}
import edu.gemini.seqexec.server.gcal.BinaryOnOff

/**
  * Created by jluhrs on 3/14/17.
  */
class GcalEpics(epicsService: CaService, tops: Map[String, String]) {

  import EpicsCommand.setParameter
  import GcalEpics._

  val GCAL_TOP = tops.get("gc").getOrElse("")

  def post: SeqAction[Unit] = lampsCmd.post

  object shutterCmd extends EpicsCommand {
    override val cs = Option(epicsService.getCommandSender("gcal::shutter"))

    val position = cs.map(_.getString("position"))
    def setPosition(v: String): SeqAction[Unit] = setParameter(position, v)
  }

  object filterCmd extends EpicsCommand {
    override val cs = Option(epicsService.getCommandSender("gcal::filtSel"))

    val name = cs.map(_.getString("name"))
    def setName(v: String): SeqAction[Unit] = setParameter(name, v)
  }

  object diffuserCmd extends EpicsCommand {
    override val cs = Option(epicsService.getCommandSender("gcal::diffuseSel"))

    val name = cs.map(_.getString("name"))
    def setName(v: String): SeqAction[Unit] = setParameter(name, v)
  }

  private def toLampState(v: BinaryOnOff): String = v.name

  object lampsCmd extends EpicsCommand {
    override val cs = Option(epicsService.getCommandSender("gcal::lampSel"))

    val nameAr = cs.map(_.getString("nameAr"))
    def setArLampName(v: String): SeqAction[Unit] = setParameter(nameAr, v)

    val stateAr = cs.map(_.getString("stateAr"))
    def setArLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateAr, v, toLampState)

    val nameCuAr = cs.map(_.getString("nameCuAr"))
    def setCuArLampName(v: String): SeqAction[Unit] = setParameter(nameCuAr, v)

    val stateCuAr = cs.map(_.getString("stateCuAr"))
    def setCuArLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateCuAr, v, toLampState)

    val nameIR = cs.map(_.getString("nameIR"))
    def setIRLampName(v: String): SeqAction[Unit] = setParameter(nameIR, v)

    val stateIR = cs.map(_.getString("stateIR"))
    def setIRLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateIR, v, toLampState)

    val nameQH = cs.map(_.getString("nameQH"))
    def setQHLampName(v: String): SeqAction[Unit] = setParameter(nameQH, v)

    val stateQH = cs.map(_.getString("stateQH"))
    def setQHLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateQH, v, toLampState)

    val nameXe = cs.map(_.getString("nameXe"))
    def setXeLampName(v: String): SeqAction[Unit] = setParameter(nameXe, v)

    val stateXe = cs.map(_.getString("stateXe"))
    def setXeLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateXe, v, toLampState)

    val nameThAr = cs.map(_.getString("nameThAr"))
    def setThArLampName(v: String): SeqAction[Unit] = setParameter(nameThAr, v)

    val stateThAr = cs.map(_.getString("stateThAr"))
    def setThArLampOn(v: BinaryOnOff): SeqAction[Unit] = setParameter(stateThAr, v, toLampState)
  }

  val state = epicsService.getStatusAcceptor("gcal::status")

  def createLampAttribute(name: String, longName: String): EnumAttribute[BinaryOnOff] =
    new EnumAttribute[BinaryOnOff](state, name + "LampState", GCAL_TOP + name + "_LampState", longName + " lamp state")(classOf[BinaryOnOff])

  val lampAr: EnumAttribute[BinaryOnOff] = createLampAttribute("Ar", "Argon")

  val lampQH: EnumAttribute[BinaryOnOff] = createLampAttribute("QH", "Quartz Halogen")

  val lampCuAr: EnumAttribute[BinaryOnOff] = createLampAttribute("CuAr", "Copper Argon")

  val lampXe: EnumAttribute[BinaryOnOff] = createLampAttribute("Xe", "Xenon")

  val lampThAr: EnumAttribute[BinaryOnOff] = createLampAttribute("ThAr", "Thorium Argon")

  val lampIr: EnumAttribute[BinaryOnOff] = createLampAttribute("IR", "Infrared")

  def shutter: Option[String] = Option(state.getStringAttribute("shutter").value)

  def filter: Option[String] = Option(state.getStringAttribute("filter").value)

  def diffuser: Option[String] = Option(state.getStringAttribute("diffuser").value)
}

object GcalEpics extends EpicsSystem[GcalEpics] {

  override val className = getClass.getName
  override val Log = Logger.getLogger(className)
  override val CA_CONFIG_FILE = "/Gcal.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GcalEpics(service, tops)

  class EnumAttribute[T <: Enum[T]](sa: CaStatusAcceptor, attrName: String, attrChannel: String, desc: String)(c: Class[T]) {
    private val attribute: Option[CaAttribute[T]] = Option(sa.addEnum(attrName, attrChannel, c, desc))
    def apply(): Option[T] = attribute.flatMap(v => Option(v.value))
  }

}
