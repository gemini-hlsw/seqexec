// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.implicits._
import cats.effect.IO
import cats.effect.Async
import cats.effect.Sync
import edu.gemini.epics.acm._
import java.lang.{Double => JDouble}

import mouse.all._
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.gmos.GmosEpics.{RoiParameters, RoiStatus}
import seqexec.server.{EpicsCommandBase, EpicsSystem}
import seqexec.server.EpicsUtil._
import seqexec.server.SeqexecFailure._
import seqexec.server.EpicsCommandBase
import seqexec.server.ObserveCommand

import scala.collection.breakOut
import scala.concurrent.duration._

class GmosEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  val GmosTop: String = tops.getOrElse("gm", "gm:")

  def post(timeout: FiniteDuration): F[ApplyCommandResult] = configCmd.post(timeout)

  object configCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::config"))

    val disperserMode: Option[CaParameter[String]] = cs.map(_.getString("disperserMode"))
    def setDisperserMode(v: String): F[Unit] = setParameter(disperserMode, v)

    val disperser: Option[CaParameter[String]] = cs.map(_.getString("disperser"))
    def setDisperser(v: String): F[Unit] = setParameter(disperser, v)

    val stageMode: Option[CaParameter[String]] = cs.map(_.getString("stageMode"))
    def setStageMode(v: String): F[Unit] = setParameter(stageMode, v)

    val useElectronicOffsetting: Option[CaParameter[Integer]] = cs.map(_.addInteger
    ("useElectronicOffsetting", s"${GmosTop}wfs:followA.K", "Enable electronic Offsets", false))
    def setElectronicOffsetting(v: Integer): F[Unit] = setParameter(useElectronicOffsetting, v)

    val filter1: Option[CaParameter[String]] = cs.map(_.getString("filter1"))
    def setFilter1(v: String): F[Unit] = setParameter(filter1, v)

    val filter2: Option[CaParameter[String]] = cs.map(_.getString("filter2"))
    def setFilter2(v: String): F[Unit] = setParameter(filter2, v)

    val dtaXOffset: Option[CaParameter[JDouble]] = cs.map(_.getDouble("dtaXOffset"))
    def setDtaXOffset(v: Double): F[Unit] = setParameter(dtaXOffset, JDouble.valueOf(v))

    val inBeam: Option[CaParameter[String]] = cs.map(_.getString("inbeam"))
    def setInBeam(v: String): F[Unit] = setParameter(inBeam, v)

    val disperserOrder: Option[CaParameter[String]] = cs.map(_.getString("disperserOrder"))
    def setDisperserOrder(v: String): F[Unit] = setParameter(disperserOrder, v)

    val disperserLambda: Option[CaParameter[JDouble]] = cs.map(_.getDouble("disperserLambda"))
    def setDisperserLambda(v: Double): F[Unit] = setParameter(disperserLambda, JDouble.valueOf(v))

    val fpu: Option[CaParameter[String]] = cs.map(_.getString("fpu"))
    def setFpu(v: String): F[Unit] = setParameter(fpu, v)

  }

  object endObserveCmd extends EpicsCommandBase {
    override protected val cs:Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::endObserve"))
  }

  object pauseCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::pause"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender("gmos::observeCmd",
      s"${GmosTop}apply", s"${GmosTop}applyC", s"${GmosTop}dc:observeC", false, s"${GmosTop}stop", s"${GmosTop}abort", ""))

  object continueCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::continue"))
    override protected val os: Option[CaApplySender] = observeAS
  }

  object stopCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::abort"))

  object abortCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = abortCS
  }

  object abortAndWait extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  object observeCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  object configDCCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::dcconfig"))

    private val roiNumUsed: Option[CaParameter[JDouble]] =
      cs.map(_.addDouble("roiNumUsed", s"${GmosTop}dc:roiNumrois", "Number of ROI used", false))
    def setRoiNumUsed(v: Int): F[Unit] = setParameter(roiNumUsed, java.lang.Double.valueOf(v.toDouble))

    val rois: Map[Int, RoiParameters[F]] = (1 to 5).map(i => i -> RoiParameters[F](cs, i))(breakOut)

    private val shutterState: Option[CaParameter[String]] =
      cs.map(_.getString("shutterState"))
    def setShutterState(v: String): F[Unit] = setParameter(shutterState, v)

    private val exposureTime: Option[CaParameter[JDouble]] =
      cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Duration): F[Unit] = setParameter(exposureTime, JDouble.valueOf(v.toSeconds.toDouble))

    private val ampCount: Option[CaParameter[String]] =
      cs.map(_.getString("ampCount"))
    def setAmpCount(v: String): F[Unit] = setParameter(ampCount, v)

    private val ampReadMode: Option[CaParameter[String]] =
      cs.map(_.getString("ampReadMode"))
    def setAmpReadMode(v: String): F[Unit] = setParameter(ampReadMode, v)

    private val gainSetting: Option[CaParameter[Integer]] =
      cs.map(_.getInteger("gainSetting"))
    def setGainSetting(v: Int): F[Unit] = setParameter(gainSetting, Integer.valueOf(v))

    private val ccdXBinning: Option[CaParameter[JDouble]] =
      cs.map(_.addDouble("ccdXBinning", s"${GmosTop}dc:roiXBin", "CCD X Binning Value", false))
    def setCcdXBinning(v: Int): F[Unit] = setParameter(ccdXBinning, java.lang.Double.valueOf(v.toDouble))

    private val ccdYBinning: Option[CaParameter[JDouble]] =
      cs.map(_.addDouble("ccdYBinning", s"${GmosTop}dc:roiYBin", "CCD Y Binning Value", false))
    def setCcdYBinning(v: Int): F[Unit] = setParameter(ccdYBinning, java.lang.Double.valueOf(v.toDouble))

    private val nsPairs: Option[CaParameter[Integer]] =
      cs.map(_.getInteger("nsPairs"))
    def setNsPairs(v: Integer): F[Unit] = setParameter(nsPairs, v)

    private val nsRows: Option[CaParameter[Integer]] =
      cs.map(_.getInteger("nsRows"))
    def setNsRows(v: Integer): F[Unit] = setParameter(nsRows, v)

    private val nsState: Option[CaParameter[String]] =
      cs.map(_.getString("ns_state"))
    def setNsState(v: String): F[Unit] = setParameter(nsState, v)

  }

  val dcState: CaStatusAcceptor = epicsService.getStatusAcceptor("gmos::dcstatus")
  private def dcRead(name: String): F[String] =
    safeAttributeF(dcState.getStringAttribute(name))

  private def dcReadI(name: String): F[Int] =
    safeAttributeSIntF[F](dcState.getIntegerAttribute(name))

  private def dcReadD(name: String): F[Double] =
    safeAttributeSDoubleF[F](dcState.getDoubleAttribute(name))

  // DC status values

  def roiNumUsed: F[Int] = dcReadI("detnroi")

  def rois: F[Map[Int, RoiStatus[F]]] =
    Sync[F].delay((1 to 5).map(i => i -> RoiStatus[F](dcState, i)).toMap)

  def ccdXBinning: F[Int] = dcReadD("ccdXBinning").map(_.toInt)

  def ccdYBinning: F[Int] = dcReadD("ccdYBinning").map(_.toInt)

  def currentCycle: F[Int] = dcReadI("currentCycle")

  def nsRows: F[Int] = dcReadI("nsRows")

  def nsPairs: F[Int] = dcReadI("nsPairs")

  def dhsConnected: F[String] = dcRead("dhsConnected")

  def countdown: F[Double] = dcReadD("countdown")

  def gainSetting: F[Int] = dcReadI("gainSetting")

  def aExpCount: F[Int] = dcReadI("aexpcnt")

  def bExpCount: F[Int] = dcReadI("bexpcnt")

  def ampCount: F[String] = dcRead("ampCount")

  def shutterState: F[String] = dcRead("shutterState")

  def ampReadMode: F[String] = dcRead("ampReadMode")

  def nsState: F[String] = dcRead("ns_state")

  def exposureTime: F[Int] = dcReadI("exposureTime")

  def reqExposureTime: F[Int] = dcReadI("exposure")

  def detectorId: F[String] = dcRead("detid")

  def detectorType: F[String] = dcRead("dettype")

  def dcName: F[String] = dcRead("gmosdc")

  def dcIsPreparing: F[Boolean] = dcReadI("obsPrep").map(_ =!= 0)

  def dcIsAcquiring: F[Boolean] = dcReadI("obsAcq").map(_ =!= 0)

  def dcIsReadingOut: F[Boolean] = dcReadI("readingOut").map(_ =!= 0)

  private val observeCAttr: CaAttribute[CarState] = dcState.addEnum("observeC",
    s"${GmosTop}dc:observeC", classOf[CarState])
  def observeState: F[CarState] = safeAttributeF(observeCAttr)

  // CC status values
  val state: CaStatusAcceptor = epicsService.getStatusAcceptor("gmos::status")
  private def read(name: String): F[String] =
    safeAttributeF(state.getStringAttribute(name))

  private def readI(name: String): F[Int] =
    safeAttributeSIntF[F](state.getIntegerAttribute(name))

  private def readD(name: String): F[Double] =
    safeAttributeSDoubleF[F](state.getDoubleAttribute(name))

  def ccName: F[String] = read("gmoscc")

  def adcPrismExitAngleStart: F[Double] = readD("adcexpst")

  def adcPrismExitAngleEnd: F[Double] = readD("adcexpen")

  def adcExitUpperWavel: F[Double] = readD("adcwlen2")

  def adcUsed: F[Int] = readI("adcused")

  def adcExitLowerWavel: F[Double] = readD("adcwlen1")

  def inBeam: F[Int] = readI("inbeam")

  def filter1Id: F[Int] = readI("filterID1")

  def filter2Id: F[Int] = readI("filterID2")

  def fpu: F[String] = read("fpu")

  def disperserMode: F[Int] = readI("disperserMode")

  def disperserInBeam: F[Int] = readI("disperserInBeam")

  def disperserOrder: F[Int] = readI("disperserOrder")

  def disperserParked: F[Boolean] = readI("disperserParked")
    .map(_ =!= 0)

  def disperserId: F[Int] = readI("disperserID")

  def filter1: F[String] = read("filter1")

  def filter2: F[String] = read("filter2")

  def disperser: F[String] = read("disperser")

  def stageMode: F[String] = read("stageMode")

  def electronicOffset: F[Int] = readI("useElectronicOffsetting")

  def disperserWavel: F[Double] = readD("disperserLambda")

  def adcMode: F[String] = read("adcmode")

  def reqGratingMotorSteps: F[Double] = readD("grstep")

  def dtaZStart: F[Double] = readD("dtazst")

  def dtaZMean: F[Double] = readD("dtazme")

  def dtaZEnd: F[Double] = readD("dtazen")

  def dtaZ: F[Double] = readD("dtaz")

  def dtaY: F[Double] = readD("dtay")

  def dtaX: F[Double] = readD("dtax")

  def dtaXOffset: F[Double] = readD("dtaXOffset")

  def dtaXCenter: F[Double] = read("dtaXCenter")
    .map(_.parseDoubleOption)
    .ensure(NullEpicsError("dtaXCenter"))(_.isDefined) // equivalent to a null check
    .map{_.getOrElse(0.0)} // getOrElse lets us typecheck but it will never be used due to the `ensure` call above

  def gratingWavel: F[Double] = readD("adjgrwlen")

  def adcPrismEntryAngleEnd: F[Double] = readD("adcenpen")

  def adcPrismEntryAngleMean: F[Double] = readD("adcenpme")

  def adcPrismEntryAngleStart: F[Double] = readD("adcenpst")

  def maskType: F[Int] = readI("masktyp")

  def maskId: F[Int] = readI("maskid")

  def gratingTilt: F[Double] = readD("grtilt")
}

object GmosEpics extends EpicsSystem[GmosEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gmos.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GmosEpics[IO]] =
    Sync[F].delay(new GmosEpics(service, tops))

  final case class RoiParameters[F[_]: Sync](cs: Option[CaCommandSender], i: Int) {
    val ccdXstart: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdXstart$i"))
    def setCcdXstart1(v: Integer): F[Unit] = setParameter(ccdXstart, v)

    val ccdYstart: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdYstart$i"))
    def setCcdYstart1(v: Integer): F[Unit] = setParameter(ccdYstart, v)

    val ccdXsize: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdXsize$i"))
    def setCcdXsize1(v: Integer): F[Unit] = setParameter(ccdXsize, v)

    val ccdYsize: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdYsize$i"))
    def setCcdYsize1(v: Integer): F[Unit] = setParameter(ccdYsize, v)
  }

  final case class RoiStatus[F[_]: Sync](sa: CaStatusAcceptor, i: Int) {
    private def readI(name: String): F[Int] =
      safeAttributeSIntF[F](sa.getIntegerAttribute(name))

    def ccdXstart: F[Int] = readI(s"ccdXstart$i")
    def ccdYstart: F[Int] = readI(s"ccdYstart$i")
    def ccdXsize: F[Int] = readI(s"ccdXsize$i")
    def ccdYsize: F[Int] = readI(s"ccdYsize$i")
  }

}
