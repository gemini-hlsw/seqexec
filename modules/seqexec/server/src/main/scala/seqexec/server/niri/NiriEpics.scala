// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import java.lang.{Double => JDouble}

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.niri.{Disperser => JDisperser}
import edu.gemini.seqexec.server.niri.{ReadMode => JReadMode}
import edu.gemini.seqexec.server.niri.{Mask => JMask}
import edu.gemini.seqexec.server.niri.{Camera => JCamera}
import edu.gemini.seqexec.server.niri.{BeamSplitter => JBeamSplitter}
import edu.gemini.seqexec.server.niri.{BuiltInROI => JBuiltInROI}
import edu.gemini.seqexec.server.niri.{DetectorState => JDetectorState}
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.{EpicsCommand, EpicsSystem, ObserveCommand, SeqAction}
import cats.implicits._
import org.log4s.{Logger, getLogger}

class NiriEpics(epicsService: CaService, tops: Map[String, String]) {

  val NIRI_TOP = tops.getOrElse("niri", "niri:")
  val NIS_TOP = tops.getOrElse("nis", "NIS:")

  object configCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nis::config"))

    val disperser: Option[CaParameter[JDisperser]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("disperser", s"${NIS_TOP}grism:menu", classOf[JDisperser], false)))
    def setDisperser(v: JDisperser): SeqAction[Unit] = setParameter(disperser, v)

    val readMode: Option[CaParameter[JReadMode]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("readmode", s"${NIS_TOP}readmode:menu", classOf[JReadMode], false)))
    def setReadMode(v: JReadMode): SeqAction[Unit] = setParameter(readMode, v)

    val coadds: Option[CaParameter[Integer]] = cs.flatMap(cmd => Option(cmd.getInteger("numCoAdds")))
    def setCoadds(v: Int): SeqAction[Unit] = setParameter(coadds, Integer.valueOf(v))

    val mask: Option[CaParameter[JMask]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("mask", s"${NIS_TOP}fpmask:menu", classOf[JMask], false)))
    def setMask(v: JMask): SeqAction[Unit] = setParameter(mask, v)

    val camera: Option[CaParameter[JCamera]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("camera", s"${NIS_TOP}camera:menu", classOf[JCamera], false)))
    def setCamera(v: JCamera): SeqAction[Unit] = setParameter(camera, v)

    val beamSplitter: Option[CaParameter[JBeamSplitter]] = cs.flatMap(cmd => Option(
      cmd.addEnum("beamSplitter", s"${NIS_TOP}beamsplit:menu", classOf[JBeamSplitter], false)))
    def setBeamSplitter(v: JBeamSplitter): SeqAction[Unit] = setParameter(beamSplitter, v)

    val exposureTime: Option[CaParameter[JDouble]] = cs.flatMap(cmd =>
      Option(cmd.getDouble("exposureTime")))
    def setExposureTime(v: Double): SeqAction[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    val builtInROI: Option[CaParameter[JBuiltInROI]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("builtinROI", s"${NIS_TOP}roi:menu", classOf[JBuiltInROI], false)))
    def setBuiltInROI(v: JBuiltInROI): SeqAction[Unit] = setParameter(builtInROI, v)

    val filter: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString("filter")))
    def setFilter(v: String): SeqAction[Unit] = setParameter(filter, v)

    val focus: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString
    ("focus")))
    def setFocus(v: String): SeqAction[Unit] = setParameter(focus, v)

  }

  /*
   * For some reason the window cover is not include in the IS configuration parameters. It is
   * applied by the IS apply command, nevertheless. This command exists only to set the parameter.
   */
  object windowCoverConfig extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri:config"))

    val windowCover: Option[CaParameter[String]] = cs.flatMap(cmd =>
      Option(cmd.getString("windowCover")))
    def setWindowCover(v: String): SeqAction[Unit] = setParameter(windowCover, v)
  }

  object endObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("niri::endObserve"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "niri::observeCmd", s"${NIRI_TOP}dc:apply", s"${NIRI_TOP}dc:applyC", s"${NIRI_TOP}dc:observeC",
    true, s"${NIRI_TOP}dc:stop", s"${NIRI_TOP}dc:abort", ""))

  object stopCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::abort"))

  object abortCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
  }

  object abortAndWait extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  object observeCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("niri::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)
  }

  val status: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::status")

  def beamSplitter: Option[String] = Option(status.getStringAttribute("BEAMSPLT").value)

  def focus: Option[String] = Option(status.getStringAttribute("FOCUSNAM").value)

  def focusPosition: Option[Double] = Option(status.getDoubleAttribute("FOCUSPOS").value)
    .map(_.toDouble)

  def mask: Option[String] = Option(status.getStringAttribute("FPMASK").value)

  def pupilViewer: Option[String] = Option(status.getStringAttribute("PVIEW").value)

  def camera: Option[String] = Option(status.getStringAttribute("CAMERA").value)

  def windowCover: Option[String] = Option(status.getStringAttribute("WINDCOVR").value)

  def filter1: Option[String] = Option(status.getStringAttribute("FILTER1").value)

  def filter2: Option[String] = Option(status.getStringAttribute("FILTER2").value)

  def filter3: Option[String] = Option(status.getStringAttribute("FILTER3").value)

  val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::dcstatus")

  def dhsConnected: Option[Boolean] = Option(dcStatus.getIntegerAttribute("dhcConnected").value)
    .map(_.toInt === 1)

  val arrayActiveAttr: Option[CaAttribute[JDetectorState]] = Option(dcStatus.addEnum(
    "arrayState", s"${NIRI_TOP}dc:activate", classOf[JDetectorState]
  ))
  def arrayActive: Option[Boolean] = arrayActiveAttr.flatMap(at => Option(at.value))
    .map(_.getActive)

  def minIntegration: Option[Double] = Option(dcStatus.getDoubleAttribute("minInt").value)
    .map(_.toDouble)

  def integrationTime: Option[Double] = Option(dcStatus.getDoubleAttribute("intTime").value)
    .map(_.toDouble)

  def coadds: Option[Int] = Option(dcStatus.getIntegerAttribute("numCoAdds").value).map(_.toInt)

  def detectorTemp: Option[Double] = Option(dcStatus.getDoubleAttribute("TDETABS").value)
    .map(_.toDouble)

  def µcodeName: Option[String] = Option(dcStatus.getStringAttribute("UCODENAM").value)

  def µcodeType: Option[Int] = Option(dcStatus.getIntegerAttribute("UCODETYP").value)
    .map(_.toInt)

  def framesPerCycle: Option[Int] = Option(dcStatus.getIntegerAttribute("FRMSPCYCL").value)
    .map(_.toInt)

  def detectorVDetBias: Option[Double] = Option(dcStatus.getDoubleAttribute("VDET").value)
    .map(_.toDouble)

  def detectorVSetBias: Option[Double] = Option(dcStatus.getDoubleAttribute("VSET").value)
    .map(_.toDouble)

  def obsEpoch: Option[Double] = Option(dcStatus.getDoubleAttribute("OBSEPOCH").value)
    .map(_.toDouble)

  def mountTemp: Option[Double] = Option(dcStatus.getDoubleAttribute("TMOUNT").value)
    .map(_.toDouble)

  def digitalAverageCount: Option[Int] = Option(dcStatus.getIntegerAttribute("NDAVGS").value)
    .map(_.toInt)

  def vggCl1: Option[Double] = Option(dcStatus.getDoubleAttribute("VGGCL1").value).map(_.toDouble)

  def vddCl1: Option[Double] = Option(dcStatus.getDoubleAttribute("VDDCL1").value).map(_.toDouble)

  def vggCl2: Option[Double] = Option(dcStatus.getDoubleAttribute("VGGCL2").value).map(_.toDouble)

  def vddCl2: Option[Double] = Option(dcStatus.getDoubleAttribute("VDDCL2").value).map(_.toDouble)

  def vddUc: Option[Double] = Option(dcStatus.getDoubleAttribute("VDDUC").value).map(_.toDouble)

  def lnrs: Option[Int] = Option(dcStatus.getIntegerAttribute("LNRS").value).map(_.toInt)

  def hdrTiming: Option[Int] = Option(dcStatus.getIntegerAttribute("hdrtiming").value).map(_.toInt)

  def arrayType: Option[String] = Option(dcStatus.getStringAttribute("ARRAYTYP").value)

  def arrayId: Option[String] = Option(dcStatus.getStringAttribute("ARRAYID").value)

  def mode: Option[Int] = Option(dcStatus.getIntegerAttribute("MODE").value).map(_.toInt)

}

object NiriEpics extends EpicsSystem[NiriEpics] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Niri.xml"

  override def build(service: CaService, tops: Map[String, String]) = new NiriEpics(service, tops)

}