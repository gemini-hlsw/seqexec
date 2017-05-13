package edu.gemini.seqexec.server

import java.util.logging.Logger
import java.lang.{Double => JDouble}

import edu.gemini.epics.acm.{CaCommandSender, CaService, CaStatusAcceptor, XMLBuilder}
import edu.gemini.seqexec.server.EpicsCommand.setParameter
import edu.gemini.seqexec.server.GmosEpics.{RoiParameters, RoiStatus}

/**
  * Created by jluhrs on 4/20/17.
  */
class GmosEpics(epicsService: CaService, tops: Map[String, String]) {

  val GMOS_TOP = tops.getOrElse("gm", "")

  object configCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::apply"))

    val disperserMode = cs.map(_.getString("disperserMode"))
    def setDisperserMode(v: String): SeqAction[Unit] = setParameter(disperserMode, v)

    val disperser = cs.map(_.getString("disperser"))
    def setDisperser(v: String): SeqAction[Unit] = setParameter(disperser, v)

    val stageMode = cs.map(_.getString("stageMode"))
    def setStageMode(v: String): SeqAction[Unit] = setParameter(stageMode, v)

    val useElectronicOffsetting = cs.map(_.addInteger("useElectronicOffsetting", GMOS_TOP + "wfs:followA.K", "Enable electronic Offsets", false))
    def setElectronicOffsetting(v: Integer): SeqAction[Unit] = setParameter(useElectronicOffsetting, v)

    val filter1 = cs.map(_.getString("filter1"))
    def setFilter1(v: String): SeqAction[Unit] = setParameter(filter1, v)

    val filter2 = cs.map(_.getString("filter2"))
    def setFilter2(v: String): SeqAction[Unit] = setParameter(filter2, v)

    val dtaXOffset = cs.map(_.getString("dtaXOffset"))
    def setDtaXOffset(v: String): SeqAction[Unit] = setParameter(dtaXOffset, v)

    val inBeam = cs.map(_.getString("inbeam"))
    def setInBeam(v: String): SeqAction[Unit] = setParameter(inBeam, v)

    val disperserOrder = cs.map(_.getString("disperserOrder"))
    def setDisperserOrder(v: String): SeqAction[Unit] = setParameter(disperserOrder, v)

    val disperserLambda = cs.map(_.getString("disperserLambda"))
    def setDisperserLambda(v: String): SeqAction[Unit] = setParameter(disperserLambda, v)

    val fpu = cs.map(_.getString("fpu"))
    def setFpu(v: String): SeqAction[Unit] = setParameter(fpu, v)

  }

  object endObserveCmd extends EpicsCommand {
    override protected val cs:Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::endObserve"))
  }

  object pauseCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::pause"))
  }

  object continueCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::continue"))
  }

  object stopCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::stop"))
  }

  object abortCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::abort"))
  }

  object observeCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::observe"))

    val label = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)
  }

  object configDCCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gmos::dcconfig"))

    val roiNumUsed = cs.map(_.addDouble("roiNumUsed", "dc:roiNumrois", "Number of ROI used", false))
    def setRoiNumUsed(v: Int): SeqAction[Unit] = setParameter(roiNumUsed, java.lang.Double.valueOf(v))

    val roi1 = new RoiParameters(cs, 1)

    val roi2 = new RoiParameters(cs, 2)

    val roi3 = new RoiParameters(cs, 3)

    val roi4 = new RoiParameters(cs, 4)

    val roi5 = new RoiParameters(cs, 5)

    val shutterState = cs.map(_.getString("shutterState"))
    def setShutterState(v: String): SeqAction[Unit] = setParameter(shutterState, v)

    val exposureTime = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Duration): SeqAction[Unit] = setParameter(exposureTime, JDouble.valueOf(v.toSeconds))

    val ampCount = cs.map(_.getInteger("ampCount"))
    def setAmpCount(v: Integer): SeqAction[Unit] = setParameter(ampCount, v)

    val ampReadMode = cs.map(_.getString("ampReadMode"))
    def setAmpReadMode(v: String): SeqAction[Unit] = setParameter(ampReadMode, v)

    val gainSetting = cs.map(_.getString("gainSetting"))
    def setGainSetting(v: String): SeqAction[Unit] = setParameter(gainSetting, v)

    val ccdXBinning = cs.map(_.addDouble("ccdXBinning", "dc:roiXBin", "CCD X Binning Value", false))
    def setCcdXBinning(v: Int): SeqAction[Unit] = setParameter(ccdXBinning, java.lang.Double.valueOf(v))

    val ccdYBinning = cs.map(_.addDouble("ccdYBinning", "dc:roiYBin", "CCD Y Binning Value", false))
    def setCcdYBinning(v: Int): SeqAction[Unit] = setParameter(ccdYBinning, java.lang.Double.valueOf(v))

    val nsPairs = cs.map(_.getInteger("nsPairs"))
    def setNsPairs(v: Integer): SeqAction[Unit] = setParameter(nsPairs, v)

    val nsRows = cs.map(_.getInteger("nsRows"))
    def setNsRows(v: Integer): SeqAction[Unit] = setParameter(nsRows, v)

    val nsState = cs.map(_.getString("ns_state"))
    def setNsState(v: String): SeqAction[Unit] = setParameter(nsState, v)

  }

  val state = epicsService.getStatusAcceptor("gmos::status")
  val dcState = epicsService.getStatusAcceptor("gmos::dcstatus")

  // DC status values

  def roiNumUsed: Option[Int] = Option(dcState.getDoubleAttribute("detnroi").value.toInt)

  val roi1 = new RoiStatus(dcState, 1)

  val roi2 = new RoiStatus(dcState, 2)

  val roi3 = new RoiStatus(dcState, 3)

  val roi4 = new RoiStatus(dcState, 4)

  val roi5 = new RoiStatus(dcState, 5)

  def ccdXBinning: Option[Int] = Option(dcState.getDoubleAttribute("ccdXBinning").value.toInt)

  def ccdYBinning: Option[Int] = Option(dcState.getDoubleAttribute("ccdYBinning").value.toInt)

  def currentCycle: Option[Int] = Option(dcState.getIntegerAttribute("currentCycle").value.toInt)

  def nsRows: Option[Int] = Option(dcState.getIntegerAttribute("nsRows").value.toInt)

  def nsPairs: Option[Int] = Option(dcState.getIntegerAttribute("nsPairs").value.toInt)

  def dhsConnected: Option[String] = Option(dcState.getStringAttribute("dhsConnected").value)

  def countdown: Option[Double] = Option(dcState.getStringAttribute("countdown").value.toDouble)

  def gainSetting: Option[Int] = Option(dcState.getIntegerAttribute("gainSetting").value.toInt)

  def aExpCount: Option[Int] = Option(dcState.getIntegerAttribute("aexpcnt").value.toInt)

  def bExpCount: Option[Int] = Option(dcState.getIntegerAttribute("bexpcnt").value.toInt)

  def ampCount: Option[Int] = Option(dcState.getIntegerAttribute("ampCount").value.toInt)

  def shutterState: Option[String] = Option(dcState.getStringAttribute("shutterState").value)

  def ampReadMode: Option[String] = Option(dcState.getStringAttribute("ampReadMode").value)

  def nsState: Option[String] = Option(dcState.getStringAttribute("ns_state").value)

  def exposureTime: Option[Int] = Option(dcState.getIntegerAttribute("exposureTime").value.toInt)

  def reqExposureTime: Option[Int] = Option(dcState.getIntegerAttribute("exposure").value.toInt)

  def detectorId: Option[String] = Option(dcState.getStringAttribute("detid").value)

  def detectorType: Option[String] = Option(dcState.getStringAttribute("dettype").value)

  def dcName: Option[String] = Option(dcState.getStringAttribute("gmosdc").value)

  // CC status values

  def ccName: Option[String] = Option(state.getStringAttribute("gmoscc").value)

  def adcPrismExitAngleStart: Option[Double] = Option(state.getDoubleAttribute("adcexpst").value.toDouble)

  def adcPrismExitAngleEnd: Option[Double] = Option(state.getDoubleAttribute("adcexpen").value.toDouble)

  def adcExitUpperWavel: Option[Double] = Option(state.getDoubleAttribute("adcwlen2").value.toDouble)

  def adcUsed: Option[Boolean] = Option(state.getIntegerAttribute("adcused").value).map(_!=0)

  def adcExitLowerWavel: Option[Double] = Option(state.getDoubleAttribute("adcwlen1").value.toDouble)

  def inBeam: Option[Boolean] = Option(state.getIntegerAttribute("inbeam").value).map(_!=0)

  def filter1Id: Option[Int] = Option(dcState.getIntegerAttribute("filterID1").value.toInt)

  def filter2Id: Option[Int] = Option(dcState.getIntegerAttribute("filterID2").value.toInt)

  def fpu: Option[String] = Option(state.getStringAttribute("fpu").value)

  def disperserInBeam: Option[Int] = Option(dcState.getIntegerAttribute("disperserInBeam").value.toInt)

  def disperserOrder: Option[Int] = Option(dcState.getIntegerAttribute("disperserOrder").value.toInt)

  def disperserParked: Option[Int] = Option(dcState.getIntegerAttribute("disperserParked").value.toInt)

  def filter1: Option[String] = Option(state.getStringAttribute("filter1").value)

  def filter2: Option[String] = Option(state.getStringAttribute("filter2").value)

  def disperser: Option[String] = Option(state.getStringAttribute("disperser").value)

  def stageMode: Option[String] = Option(state.getStringAttribute("stageMode").value)

  def useElectronicOffsetting: Option[Boolean] = Option(state.getIntegerAttribute("useElectronicOffsetting").value).map(_!=0)

  def disperserWavel: Option[Double] = Option(state.getDoubleAttribute("disperserLambda").value.toDouble)

  def adcMode: Option[String] = Option(state.getStringAttribute("adcmode").value)

  def reqGratingMotorSteps: Option[Double] = Option(state.getDoubleAttribute("grstep").value.toDouble)

  def dtaZStart: Option[Double] = Option(state.getDoubleAttribute("dtazst").value.toDouble)

  def dtaZMean: Option[Double] = Option(state.getDoubleAttribute("dtazme").value.toDouble)

  def dtaZEnd: Option[Double] = Option(state.getDoubleAttribute("dtazen").value.toDouble)

  def dtaZ: Option[Double] = Option(state.getDoubleAttribute("dtaz").value.toDouble)

  def dtaY: Option[Double] = Option(state.getDoubleAttribute("dtay").value.toDouble)

  def dtaX: Option[Double] = Option(state.getDoubleAttribute("dtax").value.toDouble)

  def gratingWavel: Option[Double] = Option(state.getDoubleAttribute("adjgrwlen").value.toDouble)

  def adcPrismEntryAngleEnd: Option[Double] = Option(state.getDoubleAttribute("adcenpen").value.toDouble)

  def adcPrismEntryAngleMean: Option[Double] = Option(state.getDoubleAttribute("adcenpme").value.toDouble)

  def adcPrismEntryAngleStart: Option[Double] = Option(state.getDoubleAttribute("adcenpst").value.toDouble)

  def maskType: Option[Int] = Option(dcState.getIntegerAttribute("masktyp").value.toInt)

  def maskId: Option[Int] = Option(dcState.getIntegerAttribute("maskid").value.toInt)

  def gratingTilt: Option[Double] = Option(state.getDoubleAttribute("grtilt").value.toDouble)
}

object GmosEpics extends EpicsSystem[GmosEpics] {

  override val className = getClass.getName
  override val Log = Logger.getLogger(className)
  override val CA_CONFIG_FILE = "/Gmos.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GmosEpics(service, tops)

  class RoiParameters(cs: Option[CaCommandSender], i: Int) {
    val ccdXstart = cs.map(_.getInteger(s"ccdXstart$i"))
    def setCcdXstart1(v: Integer): SeqAction[Unit] = setParameter(ccdXstart, v)

    val ccdYstart = cs.map(_.getInteger(s"ccdYstart$i"))
    def setCcdYstart1(v: Integer): SeqAction[Unit] = setParameter(ccdYstart, v)

    val ccdXsize = cs.map(_.getInteger(s"ccdXsize$i"))
    def setCcdXsize1(v: Integer): SeqAction[Unit] = setParameter(ccdXsize, v)

    val ccdYsize = cs.map(_.getInteger(s"ccdYsize$i"))
    def setCcdYsize1(v: Integer): SeqAction[Unit] = setParameter(ccdYsize, v)
  }

  class RoiStatus(sa: CaStatusAcceptor, i: Int) {
    def ccdXstart: Option[Int] = Option(sa.getIntegerAttribute(s"ccdXstart$i").value.toInt)
    def ccdYstart: Option[Int] = Option(sa.getIntegerAttribute(s"ccdYstart$i").value.toInt)
    def ccdXsize: Option[Int] = Option(sa.getIntegerAttribute(s"ccdXsize$i").value.toInt)
    def ccdYsize: Option[Int] = Option(sa.getIntegerAttribute(s"ccdYsize$i").value.toInt)
  }

}
