// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import java.lang.{Double => JDouble}

import edu.gemini.epics.acm._
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.gmos.GmosEpics.{RoiParameters, RoiStatus}
import seqexec.server.{EpicsCommand, EpicsSystem, ObserveCommand, SeqAction, SeqexecFailure}
import seqexec.server.EpicsUtil._
import org.log4s.{Logger, getLogger}
import squants.Time

import scala.collection.breakOut
import scala.concurrent.duration._
import cats.implicits._

class GmosEpics(epicsService: CaService, tops: Map[String, String]) {

  val GMOS_TOP: String = tops.getOrElse("gm", "gm:")

  def post: SeqAction[EpicsCommand.Result] = configCmd.post

  object configCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::config"))

    val disperserMode: Option[CaParameter[String]] = cs.map(_.getString("disperserMode"))
    def setDisperserMode(v: String): SeqAction[Unit] = setParameter(disperserMode, v)

    val disperser: Option[CaParameter[String]] = cs.map(_.getString("disperser"))
    def setDisperser(v: String): SeqAction[Unit] = setParameter(disperser, v)

    val stageMode: Option[CaParameter[String]] = cs.map(_.getString("stageMode"))
    def setStageMode(v: String): SeqAction[Unit] = setParameter(stageMode, v)

    val useElectronicOffsetting: Option[CaParameter[Integer]] = cs.map(_.addInteger("useElectronicOffsetting", GMOS_TOP + "wfs:followA.K", "Enable electronic Offsets", false))
    def setElectronicOffsetting(v: Integer): SeqAction[Unit] = setParameter(useElectronicOffsetting, v)

    val filter1: Option[CaParameter[String]] = cs.map(_.getString("filter1"))
    def setFilter1(v: String): SeqAction[Unit] = setParameter(filter1, v)

    val filter2: Option[CaParameter[String]] = cs.map(_.getString("filter2"))
    def setFilter2(v: String): SeqAction[Unit] = setParameter(filter2, v)

    val dtaXOffset: Option[CaParameter[JDouble]] = cs.map(_.getDouble("dtaXOffset"))
    def setDtaXOffset(v: Double): SeqAction[Unit] = setParameter(dtaXOffset, JDouble.valueOf(v))

    val inBeam: Option[CaParameter[String]] = cs.map(_.getString("inbeam"))
    def setInBeam(v: String): SeqAction[Unit] = setParameter(inBeam, v)

    val disperserOrder: Option[CaParameter[String]] = cs.map(_.getString("disperserOrder"))
    def setDisperserOrder(v: String): SeqAction[Unit] = setParameter(disperserOrder, v)

    val disperserLambda: Option[CaParameter[JDouble]] = cs.map(_.getDouble("disperserLambda"))
    def setDisperserLambda(v: Double): SeqAction[Unit] = setParameter(disperserLambda, JDouble.valueOf(v))

    val fpu: Option[CaParameter[String]] = cs.map(_.getString("fpu"))
    def setFpu(v: String): SeqAction[Unit] = setParameter(fpu, v)

  }

  object endObserveCmd extends EpicsCommand {
    override protected val cs:Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::endObserve"))
  }

  object pauseCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::pause"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender("gmos::observeCmd",
      GMOS_TOP + "apply", GMOS_TOP + "applyC", GMOS_TOP + "dc:observeC", false, GMOS_TOP + "stop", GMOS_TOP + "abort", ""))

  object continueCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::continue"))
    override protected val os: Option[CaApplySender] = observeAS
  }

  object stopCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::abort"))

  object abortCmd extends EpicsCommand {
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
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)
  }

  object configDCCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::dcconfig"))

    val roiNumUsed: Option[CaParameter[JDouble]] = cs.map(_.addDouble("roiNumUsed", GMOS_TOP + "dc:roiNumrois", "Number of ROI used", false))
    def setRoiNumUsed(v: Int): SeqAction[Unit] = setParameter(roiNumUsed, java.lang.Double.valueOf(v.toDouble))

    val rois: Map[Int, RoiParameters] = (1 to 5).map(i => i -> RoiParameters(cs, i))(breakOut)

    val shutterState: Option[CaParameter[String]] = cs.map(_.getString("shutterState"))
    def setShutterState(v: String): SeqAction[Unit] = setParameter(shutterState, v)

    val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Duration): SeqAction[Unit] = setParameter(exposureTime, JDouble.valueOf(v.toSeconds.toDouble))

    val ampCount: Option[CaParameter[String]] = cs.map(_.getString("ampCount"))
    def setAmpCount(v: String): SeqAction[Unit] = setParameter(ampCount, v)

    val ampReadMode: Option[CaParameter[String]] = cs.map(_.getString("ampReadMode"))
    def setAmpReadMode(v: String): SeqAction[Unit] = setParameter(ampReadMode, v)

    val gainSetting: Option[CaParameter[Integer]] = cs.map(_.getInteger("gainSetting"))
    def setGainSetting(v: Int): SeqAction[Unit] = setParameter(gainSetting, Integer.valueOf(v))

    val ccdXBinning: Option[CaParameter[JDouble]] = cs.map(_.addDouble("ccdXBinning", GMOS_TOP + "dc:roiXBin", "CCD X Binning Value", false))
    def setCcdXBinning(v: Int): SeqAction[Unit] = setParameter(ccdXBinning, java.lang.Double.valueOf(v.toDouble))

    val ccdYBinning: Option[CaParameter[JDouble]] = cs.map(_.addDouble("ccdYBinning", GMOS_TOP + "dc:roiYBin", "CCD Y Binning Value", false))
    def setCcdYBinning(v: Int): SeqAction[Unit] = setParameter(ccdYBinning, java.lang.Double.valueOf(v.toDouble))

    val nsPairs: Option[CaParameter[Integer]] = cs.map(_.getInteger("nsPairs"))
    def setNsPairs(v: Integer): SeqAction[Unit] = setParameter(nsPairs, v)

    val nsRows: Option[CaParameter[Integer]] = cs.map(_.getInteger("nsRows"))
    def setNsRows(v: Integer): SeqAction[Unit] = setParameter(nsRows, v)

    val nsState: Option[CaParameter[String]] = cs.map(_.getString("ns_state"))
    def setNsState(v: String): SeqAction[Unit] = setParameter(nsState, v)

  }

  val state: CaStatusAcceptor = epicsService.getStatusAcceptor("gmos::status")
  val dcState: CaStatusAcceptor = epicsService.getStatusAcceptor("gmos::dcstatus")

  // DC status values

  def roiNumUsed: Option[Int] = Option(dcState.getIntegerAttribute("detnroi").value).map(_.toInt)

  val rois: Map[Int, RoiStatus] = (1 to 5).map(i => i -> RoiStatus(dcState, i))(breakOut)

  def ccdXBinning: Option[Int] = Option(dcState.getDoubleAttribute("ccdXBinning").value).map(_.toInt)

  def ccdYBinning: Option[Int] = Option(dcState.getDoubleAttribute("ccdYBinning").value).map(_.toInt)

  def currentCycle: Option[Int] = Option(dcState.getIntegerAttribute("currentCycle").value).map(_.toInt)

  def nsRows: Option[Int] = Option(dcState.getIntegerAttribute("nsRows").value).map(_.toInt)

  def nsPairs: Option[Int] = Option(dcState.getIntegerAttribute("nsPairs").value).map(_.toInt)

  def dhsConnected: Option[String] = Option(dcState.getStringAttribute("dhsConnected").value)

  def countdown: Option[Double] = Option(dcState.getStringAttribute("countdown").value).map(_.toDouble)

  def gainSetting: Option[Int] = Option(dcState.getIntegerAttribute("gainSetting").value).map(_.toInt)

  def aExpCount: Option[Int] = Option(dcState.getIntegerAttribute("aexpcnt").value).map(_.toInt)

  def bExpCount: Option[Int] = Option(dcState.getIntegerAttribute("bexpcnt").value).map(_.toInt)

  def ampCount: Option[Int] = Option(dcState.getIntegerAttribute("ampCount").value).map(_.toInt)

  def shutterState: Option[String] = Option(dcState.getStringAttribute("shutterState").value)

  def ampReadMode: Option[String] = Option(dcState.getStringAttribute("ampReadMode").value)

  def nsState: Option[String] = Option(dcState.getStringAttribute("ns_state").value)

  def exposureTime: Option[Int] = Option(dcState.getIntegerAttribute("exposureTime").value).map(_.toInt)

  def reqExposureTime: Option[Int] = Option(dcState.getIntegerAttribute("exposure").value).map(_.toInt)

  def detectorId: Option[String] = Option(dcState.getStringAttribute("detid").value)

  def detectorType: Option[String] = Option(dcState.getStringAttribute("dettype").value)

  def dcName: Option[String] = Option(dcState.getStringAttribute("gmosdc").value)

  private val observeCAttr: Option[CaAttribute[CarState]] = Option(dcState.addEnum("observeC",
    GMOS_TOP + "dc:observeC", classOf[CarState]))
  def observeC: Option[CarState] = observeCAttr.map(_.value)

  def waitObserve(t: Time): SeqAction[CarState] = observeCAttr.map(attr =>
    waitForValues(attr, List(CarState.IDLE, CarState.ERROR, CarState.PAUSED),t , "GMOS observe state")).getOrElse(
    SeqAction.fail(SeqexecFailure.Unexpected(s"Cannot access ${GMOS_TOP}dc:observeC")))

  // CC status values

  def ccName: Option[String] = Option(state.getStringAttribute("gmoscc").value)

  def adcPrismExitAngleStart: Option[Double] = Option(state.getDoubleAttribute("adcexpst").value).map(_.toDouble)

  def adcPrismExitAngleEnd: Option[Double] = Option(state.getDoubleAttribute("adcexpen").value).map(_.toDouble)

  def adcExitUpperWavel: Option[Double] = Option(state.getDoubleAttribute("adcwlen2").value).map(_.toDouble)

  def adcUsed: Option[Int] = Option(state.getIntegerAttribute("adcused").value).map(_.toInt)

  def adcExitLowerWavel: Option[Double] = Option(state.getDoubleAttribute("adcwlen1").value).map(_.toDouble)

  def inBeam: Option[Int] = Option(state.getIntegerAttribute("inbeam").value).map(_.toInt)

  def filter1Id: Option[Int] = Option(state.getIntegerAttribute("filterID1").value).map(_.toInt)

  def filter2Id: Option[Int] = Option(state.getIntegerAttribute("filterID2").value).map(_.toInt)

  def fpu: Option[String] = Option(state.getStringAttribute("fpu").value)

  def disperserMode: Option[Int] = Option(state.getIntegerAttribute("disperserMode").value).map(_.toInt)

  def disperserInBeam: Option[Int] = Option(state.getIntegerAttribute("disperserInBeam").value).map(_.toInt)

  def disperserOrder: Option[Int] = Option(state.getIntegerAttribute("disperserOrder").value).map(_.toInt)

  def disperserParked: Option[Int] = Option(state.getIntegerAttribute("disperserParked").value).map(_.toInt)

  def disperserId: Option[Int] = Option(state.getIntegerAttribute("disperserID").value).map(_.toInt)

  def filter1: Option[String] = Option(state.getStringAttribute("filter1").value)

  def filter2: Option[String] = Option(state.getStringAttribute("filter2").value)

  def disperser: Option[String] = Option(state.getStringAttribute("disperser").value)

  def stageMode: Option[String] = Option(state.getStringAttribute("stageMode").value)

  def useElectronicOffsetting: Option[Boolean] = Option(state.getIntegerAttribute("useElectronicOffsetting").value).map(_.toInt =!= 0)

  def disperserWavel: Option[Double] = Option(state.getDoubleAttribute("disperserLambda").value).map(_.toDouble)

  def adcMode: Option[String] = Option(state.getStringAttribute("adcmode").value)

  def reqGratingMotorSteps: Option[Double] = Option(state.getDoubleAttribute("grstep").value).map(_.toDouble)

  def dtaZStart: Option[Double] = Option(state.getDoubleAttribute("dtazst").value).map(_.toDouble)

  def dtaZMean: Option[Double] = Option(state.getDoubleAttribute("dtazme").value).map(_.toDouble)

  def dtaZEnd: Option[Double] = Option(state.getDoubleAttribute("dtazen").value).map(_.toDouble)

  def dtaZ: Option[Double] = Option(state.getDoubleAttribute("dtaz").value).map(_.toDouble)

  def dtaY: Option[Double] = Option(state.getDoubleAttribute("dtay").value).map(_.toDouble)

  def dtaX: Option[Double] = Option(state.getDoubleAttribute("dtax").value).map(_.toDouble)

  def gratingWavel: Option[Double] = Option(state.getDoubleAttribute("adjgrwlen").value).map(_.toDouble)

  def adcPrismEntryAngleEnd: Option[Double] = Option(state.getDoubleAttribute("adcenpen").value).map(_.toDouble)

  def adcPrismEntryAngleMean: Option[Double] = Option(state.getDoubleAttribute("adcenpme").value).map(_.toDouble)

  def adcPrismEntryAngleStart: Option[Double] = Option(state.getDoubleAttribute("adcenpst").value).map(_.toDouble)

  def maskType: Option[Int] = Option(state.getIntegerAttribute("masktyp").value).map(_.toInt)

  def maskId: Option[Int] = Option(state.getIntegerAttribute("maskid").value).map(_.toInt)

  def gratingTilt: Option[Double] = Option(state.getDoubleAttribute("grtilt").value).map(_.toDouble)
}

object GmosEpics extends EpicsSystem[GmosEpics] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Gmos.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GmosEpics(service, tops)

  final case class RoiParameters(cs: Option[CaCommandSender], i: Int) {
    val ccdXstart: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdXstart$i"))
    def setCcdXstart1(v: Integer): SeqAction[Unit] = setParameter(ccdXstart, v)

    val ccdYstart: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdYstart$i"))
    def setCcdYstart1(v: Integer): SeqAction[Unit] = setParameter(ccdYstart, v)

    val ccdXsize: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdXsize$i"))
    def setCcdXsize1(v: Integer): SeqAction[Unit] = setParameter(ccdXsize, v)

    val ccdYsize: Option[CaParameter[Integer]] = cs.map(_.getInteger(s"ccdYsize$i"))
    def setCcdYsize1(v: Integer): SeqAction[Unit] = setParameter(ccdYsize, v)
  }

  final case class RoiStatus(sa: CaStatusAcceptor, i: Int) {
    def ccdXstart: Option[Int] = Option(sa.getIntegerAttribute(s"ccdXstart$i").value).map(_.toInt)
    def ccdYstart: Option[Int] = Option(sa.getIntegerAttribute(s"ccdYstart$i").value).map(_.toInt)
    def ccdXsize: Option[Int] = Option(sa.getIntegerAttribute(s"ccdXsize$i").value).map(_.toInt)
    def ccdYsize: Option[Int] = Option(sa.getIntegerAttribute(s"ccdYsize$i").value).map(_.toInt)
  }

}
