// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import scala.concurrent.duration.FiniteDuration

import cats.effect.Async
import cats.effect.IO
import cats.effect.Sync
import edu.gemini.epics.acm.CaAttribute
import edu.gemini.epics.acm.CaCommandSender
import edu.gemini.epics.acm.CaService
import edu.gemini.seqexec.server.gcal.BinaryOnOff
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.EpicsCommandBase
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsSystem
import seqexec.server.EpicsUtil.safeAttributeF

/**
 * Created by jluhrs on 3/14/17.
 */
class GcalEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {
  val sysName: String = "GCAL"
  val GcalTop: String = tops.getOrElse("gc", "")

  def post(timeout: FiniteDuration): F[ApplyCommandResult] = lampsCmd.post(timeout)

  object shutterCmd extends EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gcal::shutter")
    )

    private val position = cs.map(_.getString("position"))
    def setPosition(v: String): F[Unit] = setParameter(position, v)
  }

  object filterCmd extends EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gcal::filtSel")
    )

    private val name = cs.map(_.getString("name"))
    def setName(v: String): F[Unit] = setParameter(name, v)
  }

  object diffuserCmd extends EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gcal::diffuseSel")
    )

    private val name = cs.map(_.getString("name"))
    def setName(v: String): F[Unit] = setParameter(name, v)
  }

  private def toLampState(v: BinaryOnOff): String = v.name

  object lampsCmd extends EpicsCommandBase[F](sysName) {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gcal::lampSel")
    )

    private val nameAr = cs.map(_.getString("nameAr"))
    def setArLampName(v: String): F[Unit] = setParameter(nameAr, v)

    private val stateAr = cs.map(_.getString("stateAr"))
    def setArLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateAr, v, toLampState)

    private val nameCuAr = cs.map(_.getString("nameCuAr"))
    def setCuArLampName(v: String): F[Unit] = setParameter(nameCuAr, v)

    private val stateCuAr = cs.map(_.getString("stateCuAr"))
    def setCuArLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateCuAr, v, toLampState)

    private val nameIR = cs.map(_.getString("nameIR"))
    def setIRLampName(v: String): F[Unit] = setParameter(nameIR, v)

    private val stateIR = cs.map(_.getString("stateIR"))
    def setIRLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateIR, v, toLampState)

    private val nameQH5W = cs.map(_.getString("nameQH5W"))
    def setQH5WLampName(v: String): F[Unit] = setParameter(nameQH5W, v)

    private val stateQH5W = cs.map(_.getString("stateQH5W"))
    def setQH5WLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateQH5W, v, toLampState)

    private val nameQH100W = cs.map(_.getString("nameQH100W"))
    def setQH100WLampName(v: String): F[Unit] = setParameter(nameQH100W, v)

    private val stateQH100W = cs.map(_.getString("stateQH100W"))
    def setQH100WLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateQH100W, v, toLampState)

    private val nameXe = cs.map(_.getString("nameXe"))
    def setXeLampName(v: String): F[Unit] = setParameter(nameXe, v)

    private val stateXe = cs.map(_.getString("stateXe"))
    def setXeLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateXe, v, toLampState)

    private val nameThAr = cs.map(_.getString("nameThAr"))
    def setThArLampName(v: String): F[Unit] = setParameter(nameThAr, v)

    private val stateThAr = cs.map(_.getString("stateThAr"))
    def setThArLampOn(v: BinaryOnOff): F[Unit] = setParameter(stateThAr, v, toLampState)
  }

  private val state = epicsService.getStatusAcceptor("gcal::status")

  def createLampAttribute(name: String, longName: String): CaAttribute[BinaryOnOff] =
    state.addEnum[BinaryOnOff](
      name + "LampState",
      s"$GcalTop${name}_LampState",
      classOf[BinaryOnOff],
      s"$longName lamp state"
    )

  private val lampArAttr: CaAttribute[BinaryOnOff] = createLampAttribute("Ar", "Argon")
  def lampAr: F[BinaryOnOff]                       = safeAttributeF(lampArAttr)

  private val lampQH5WAttr: CaAttribute[BinaryOnOff] = createLampAttribute("QH", "Quartz Halogen")
  def lampQH5W: F[BinaryOnOff]                       = safeAttributeF(lampQH5WAttr)

  private val lampQH100WAttr: CaAttribute[BinaryOnOff] =
    createLampAttribute("QH100", "Quartz Halogen")
  def lampQH100W: F[BinaryOnOff]                       = safeAttributeF(lampQH100WAttr)

  private val lampCuArAttr: CaAttribute[BinaryOnOff] = createLampAttribute("CuAr", "Copper Argon")
  def lampCuAr: F[BinaryOnOff]                       = safeAttributeF(lampCuArAttr)

  private val lampXeAttr: CaAttribute[BinaryOnOff] = createLampAttribute("Xe", "Xenon")
  def lampXe: F[BinaryOnOff]                       = safeAttributeF(lampXeAttr)

  private val lampThArAttr: CaAttribute[BinaryOnOff] = createLampAttribute("ThAr", "Thorium Argon")
  def lampThAr: F[BinaryOnOff]                       = safeAttributeF(lampThArAttr)

  private val lampIrAttr: CaAttribute[BinaryOnOff] = createLampAttribute("IR", "Infrared")
  def lampIr: F[BinaryOnOff]                       = safeAttributeF(lampIrAttr)

  def shutter: F[String] =
    safeAttributeF(state.getStringAttribute("shutter"))

  def filter: F[String] =
    safeAttributeF(state.getStringAttribute("filter"))

  def diffuser: F[String] =
    safeAttributeF(state.getStringAttribute("diffuser"))
}

object GcalEpics extends EpicsSystem[GcalEpics[IO]] {

  override val className: String      = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gcal.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GcalEpics[IO]] =
    Sync[F].delay(new GcalEpics[IO](service, tops))

}
