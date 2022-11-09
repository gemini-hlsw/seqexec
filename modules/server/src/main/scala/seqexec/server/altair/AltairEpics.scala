// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative

import scala.concurrent.duration.FiniteDuration
import cats.effect.Async
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.altair.LgsSfoControl
import mouse.boolean._
import seqexec.model.`enum`.ApplyCommandResult
import seqexec.server.{ EpicsCommand, EpicsCommandBase, EpicsSystem, EpicsUtil }
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil._

trait AltairEpics[F[_]] {
  import AltairEpics._

  // Commands
  val strapGateControl: StrapGateControlCommand[F]
  val strapControl: StrapControlCommand[F]
  val sfoControl: SfoControlCommand[F]
  val btoLoopControl: BtoLoopControlCommand[F]

  def waitForStrapGate(v:    Int, timeout:          FiniteDuration): F[Unit]
  def waitForStrapLoop(v:    Boolean, timeout:      FiniteDuration): F[Unit]
  def waitAoSettled(timeout: FiniteDuration): F[Unit]
  def waitMatrixCalc(v:      CarStateGEM5, timeout: FiniteDuration): F[Unit]

  // Statuses
  def strapTempStatus: F[Boolean]
  def strapGate: F[Int]
  def strapLoop: F[Boolean]
  def strapRTStatus: F[Boolean]
  def strapHVStatus: F[Boolean]
  def sfoLoop: F[LgsSfoControl]
  def aoexpt: F[Float]
  def aocounts: F[Double]
  def aoseeing: F[Float]
  def aowfsx: F[Double]
  def aowfsy: F[Double]
  def aowfsz: F[Double]
  def aogain: F[Double]
  def aoncpa: F[String]
  def ngndfilt: F[String]
  def astar: F[String]
  def aoflex: F[String]
  def lgustage: F[String]
  def aobs: F[String]
  def aoLoop: F[Boolean]
  def aoSettled: F[Boolean]
  def matrixStartX: F[Double]
  def matrixStartY: F[Double]
  def controlMatrixCalc: F[CarStateGEM5]
  def lgsP1: F[Boolean]
  def lgsOi: F[Boolean]
  def aoFollow: F[Boolean]
  def lgdfocus: F[Double]
  def apd1: F[Float]
  def apd2: F[Float]
  def apd3: F[Float]
  def apd4: F[Float]
  def lgttexp: F[Int]
  def fsmtip: F[Double]
  def fsmtilt: F[Double]
  def lgzmpos: F[Double]
  def aozoom: F[Double]
  def aoza: F[Double]
  def nathick: F[Double]
  def lgndfilt: F[String]
  def lgttiris: F[String]
  def lgsfcnts: F[Double]
  def lgsfexp: F[Double]
}

class AltairEpicsImpl[F[_]: Async](service: CaService, tops: Map[String, String])
    extends AltairEpics[F] {
  import AltairEpics._

  val sysName: String   = "Altair"
  val AltairTop: String = tops.getOrElse("ao", "ao:")

  override val strapGateControl: StrapGateControlCommand[F] = new EpicsCommandBase[F](sysName)
    with StrapGateControlCommand[F] {
    override protected val cs: Option[CaCommandSender] = Option(
      service.createTaskControlSender("aoStrap", s"${AltairTop}wfcs:strapGtCtl", "ALTAIR STRAP")
    )

    val gate: Option[CaParameter[Integer]] =
      cs.map(_.addInteger("gate", s"${AltairTop}wfcs:strapGtCtl.A", "Gate control", false))
    def setGate(v: Int): F[Unit] = setParameter(gate, Integer.valueOf(v))
  }

  override val strapControl: StrapControlCommand[F] = new EpicsCommandBase[F](sysName)
    with StrapControlCommand[F] {
    override protected val cs: Option[CaCommandSender] = Option(
      service.createTaskControlSender("strapCorrCtl",
                                      s"${AltairTop}wfcs:strapCorrCtl",
                                      "ALTAIR SFO"
      )
    )

    val active: Option[CaParameter[Integer]] = cs.map(
      _.addInteger("onoff", s"${AltairTop}wfcs:strapCorrCtl.A", "Strap onoff loop control", false)
    )
    def setActive(v: Int): F[Unit] = setParameter(active, Integer.valueOf(v))
  }

  // sfoControl is a bit weird, in that changing the 'active' parameter takes effect immediately.
  // post and mark don't have any effect
  override val sfoControl: SfoControlCommand[F] = new SfoControlCommand[F] {
    private val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("aoSfoLoop"))

    private val active: Option[CaParameter[LgsSfoControl]] = cs.map(
      _.addEnum[LgsSfoControl]("active",
                               s"${AltairTop}cc:lgszoomSfoLoop.VAL",
                               classOf[LgsSfoControl],
                               false
      )
    )
    def setActive(v: LgsSfoControl): F[Unit] = setParameter(active, v)

    override def post(timeout: FiniteDuration): F[ApplyCommandResult] =
      ApplyCommandResult.Completed.pure[F].widen

    override def mark: F[Unit] = Applicative[F].unit
  }

  override val btoLoopControl: BtoLoopControlCommand[F] = new EpicsCommandBase[F](sysName)
    with BtoLoopControlCommand[F] {
    override protected val cs: Option[CaCommandSender] =
      Option(service.getCommandSender("btoFsaLoopCtrl"))

    val active: Option[CaParameter[String]] = cs.map(_.getString("loop"))
    def setActive(v: String): F[Unit] = setParameter(active, v)
  }

  val status: CaStatusAcceptor = service.getStatusAcceptor("aostate")

  override def strapTempStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapTPStat")).map(_ =!= 0)

  private val strapGateAttr      = status.getIntegerAttribute("strapgate")
  override def strapGate: F[Int] = safeAttributeSIntF(strapGateAttr)

  override def waitForStrapGate(v: Int, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(strapGateAttr, v: Integer, timeout, "Altair strap gate")

  private val strapLoopAttr          = status.getIntegerAttribute("straploop")
  override def strapLoop: F[Boolean] =
    safeAttributeSIntF(strapLoopAttr).map(_ =!= 0)

  override def waitForStrapLoop(v: Boolean, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(strapLoopAttr, v.fold(1, 0): Integer, timeout, "Altair strap loop")

  override def strapRTStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapRTStat")).map(_ =!= 0)

  override def strapHVStatus: F[Boolean] =
    safeAttributeSIntF(status.getIntegerAttribute("strapHVStat")).map(_ =!= 0)

  private val sfoLoopAttr: CaAttribute[LgsSfoControl] =
    status.addEnum("sfoloop", s"${AltairTop}cc:lgszoomSfoLoop.VAL", classOf[LgsSfoControl])
  override def sfoLoop: F[LgsSfoControl]              = safeAttributeF(sfoLoopAttr)

  override def aoexpt: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("aoexpt"))

  override def aocounts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aocounts"))

  override def aoseeing: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("aoseeing"))

  override def aowfsx: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsx"))

  override def aowfsy: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsy"))

  override def aowfsz: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aowfsz"))

  override def aogain: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aogain"))

  override def aoncpa: F[String] = safeAttributeF(status.getStringAttribute("aoncpa"))

  override def ngndfilt: F[String] = safeAttributeF(status.getStringAttribute("ngndfilt"))

  override def astar: F[String] = safeAttributeF(status.getStringAttribute("astar"))

  override def aoflex: F[String] = safeAttributeF(status.getStringAttribute("aoflex"))

  override def lgustage: F[String] = safeAttributeF(status.getStringAttribute("lgustage"))

  override def aobs: F[String] = safeAttributeF(status.getStringAttribute("aobs"))

  override def aoLoop: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("aowfsOn"))
    .map(_ =!= 0)

  private val aoSettledAttr          = status.getDoubleAttribute("aoSettled")
  override def aoSettled: F[Boolean] = safeAttributeSDoubleF(aoSettledAttr)
    .map(_ =!= 0.0)

  override def waitAoSettled(timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF[java.lang.Double, F](aoSettledAttr, 1.0, timeout, "AO settled flag")

  override def matrixStartX: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("conmatx"))

  override def matrixStartY: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("conmaty"))

  private val controlMatrixCalcAttr               =
    status.addEnum[CarStateGEM5]("cmPrepBusy", s"${AltairTop}prepareCm.BUSY", classOf[CarStateGEM5])
  override def controlMatrixCalc: F[CarStateGEM5] = safeAttributeF(controlMatrixCalcAttr)

  override def waitMatrixCalc(v: CarStateGEM5, timeout: FiniteDuration): F[Unit] =
    EpicsUtil.waitForValueF(controlMatrixCalcAttr, v, timeout, "Altair control matrix calculation")

  override def lgsP1: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("lgsp1On"))
    .map(_ =!= 0)

  override def lgsOi: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("lgsoiOn"))
    .map(_ =!= 0)

  override def aoFollow: F[Boolean] = safeAttributeF(status.getStringAttribute("aoFollowS"))
    .map(_ === "On")

  // Lgs channels
  override def lgdfocus: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgdfocus"))

  override def apd1: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd1"))

  override def apd2: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd2"))

  override def apd3: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd3"))

  override def apd4: F[Float] = safeAttributeSFloatF(status.getFloatAttribute("apd4"))

  override def lgttexp: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("lgttexp"))

  override def fsmtip: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("fsmtip"))

  override def fsmtilt: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("fsmtilt"))

  override def lgzmpos: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("lgzmpos"))

  override def aozoom: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aozoom"))

  override def aoza: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("aoza"))

  override def nathick: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("nathick"))

  override def lgndfilt: F[String] = safeAttributeF(status.getStringAttribute("lgndfilt"))

  override def lgttiris: F[String] = safeAttributeF(status.getStringAttribute("lgttiris"))

  val sfoStatus: CaStatusAcceptor = service.getStatusAcceptor("sfostate")

  override def lgsfcnts: F[Double] = safeAttributeSDoubleF(sfoStatus.getDoubleAttribute("lgsfcnts"))

  override def lgsfexp: F[Double] = safeAttributeSDoubleF(sfoStatus.getDoubleAttribute("lgsfexp"))

}

object AltairEpics extends EpicsSystem[AltairEpics[IO]] {

  override val className: String      = getClass.getName
  override val CA_CONFIG_FILE: String = "/Altair.xml"

  override def build[F[_]: Sync](
    service: CaService,
    tops:    Map[String, String]
  ): F[AltairEpics[IO]] =
    Sync[F].delay(new AltairEpicsImpl(service, tops))

  trait StrapGateControlCommand[F[_]] extends EpicsCommand[F] {
    def setGate(v: Int): F[Unit]
  }

  trait StrapControlCommand[F[_]] extends EpicsCommand[F] {
    def setActive(v: Int): F[Unit]
  }

  trait SfoControlCommand[F[_]] extends EpicsCommand[F] {
    def setActive(v: LgsSfoControl): F[Unit]
  }

  trait BtoLoopControlCommand[F[_]] extends EpicsCommand[F] {
    def setActive(v: String): F[Unit]
  }

}
