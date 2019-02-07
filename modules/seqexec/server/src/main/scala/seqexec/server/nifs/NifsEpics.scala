// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import java.lang.{Double => JDouble}

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.nifs.DhsConnected
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.{EpicsCommand, EpicsSystem, ObserveCommand, SeqAction}

class NifsEpics(epicsService: CaService, tops: Map[String, String]) {
  val NifsTop = tops.getOrElse("niri", "niri:")

  object ccConfigCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nifs::config"))

    val disperser: Option[CaParameter[String]] = cs.map(_.getString("disperser"))
    def setDisperser(v: String): SeqAction[Unit] = setParameter(disperser, v)

    val filter: Option[CaParameter[String]] = cs.map(_.getString("filter"))
    def setFilter(v: String): SeqAction[Unit] = setParameter(filter, v)

    val windowCover: Option[CaParameter[String]] = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): SeqAction[Unit] = setParameter(windowCover, v)

    val maskOffset: Option[CaParameter[String]] = cs.map(_.getString("maskOffset"))
    def setMaskOffset(v: String): SeqAction[Unit] = setParameter(maskOffset, v)

    val imagingMirror: Option[CaParameter[String]] = cs.map(_.getString("imagingMirror"))
    def setImagingMirror(v: String): SeqAction[Unit] = setParameter(imagingMirror, v)

    val mask: Option[CaParameter[String]] = cs.map(_.getString("mask"))
    def setMask(v: String): SeqAction[Unit] = setParameter(mask, v)

    val centralWavelength: Option[CaParameter[String]] = cs.map(_.getString("centralWavelength"))
    def setCentralWavelength(v: String): SeqAction[Unit] = setParameter(centralWavelength, v)

  }

  object dcConfigCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nifs::dcconfig"))

    val coadds: Option[CaParameter[Integer]] = cs.map(_.getInteger("coadds"))
    def setCoadds(v: Int): SeqAction[Unit] = setParameter(coadds, Integer.valueOf(v))

    val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): SeqAction[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    val fowlerSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfFowSamples"))
    def setFowlerSamples(v: Int): SeqAction[Unit] = setParameter(fowlerSamples, Integer.valueOf(v))

    val period: Option[CaParameter[JDouble]] = cs.map(_.getDouble("period"))
    def setPeriod(v: Double): SeqAction[Unit] = setParameter(period, JDouble.valueOf(v))

    val readMode: Option[CaParameter[Integer]] = cs.map(_.getInteger("readMode"))
    def setReadMode(v: Int): SeqAction[Unit] = setParameter(readMode, Integer.valueOf(v))

    val numberOfResets: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfResets"))
    def setnumberOfResets(v: Int): SeqAction[Unit] =
      setParameter(numberOfResets, Integer.valueOf(v))

    val numberOfPeriods: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfPeriods"))
    def setnumberOfPeriods(v: Int): SeqAction[Unit] =
      setParameter(numberOfPeriods, Integer.valueOf(v))

    val timeMode: Option[CaParameter[Integer]] = cs.map(_.getInteger("timeMode"))
    def setTimeMode(v: Int): SeqAction[Unit] = setParameter(timeMode, Integer.valueOf(v))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nifs::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "nifs::observeCmd", s"${NifsTop}dc:nifsApply", s"${NifsTop}dc:applyC", s"${NifsTop}dc:observeC",
    true, s"${NifsTop}dc:stop", s"${NifsTop}dc:abort", ""))

  object stopCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nifs::abort"))

  object abortCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
  }

  object abortAndWait extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  object observeCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("nifs::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)
  }

  object endObserveCmd extends EpicsCommand {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("nifs::endObserve"))
  }

  val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::dcstatus")

  def exposureTime: Option[Double] = Option(dcStatus.getDoubleAttribute("exposureTime").value)
    .map(_.toDouble)

  def exposedTime: Option[Double] = Option(dcStatus.getDoubleAttribute("exposedTime").value)
    .map(_.toDouble)

  def numberOfResets: Option[Int] = Option(dcStatus.getIntegerAttribute("numberOfResets").value)
    .map(_.toInt)

  def readMode: Option[String] = Option(dcStatus.getStringAttribute("readMode").value)

  def numberOfFowlerSamples: Option[Int] =
    Option(dcStatus.getIntegerAttribute("numberOfFowSamples").value).map(_.toInt)

  def coadds: Option[Int] = Option(dcStatus.getIntegerAttribute("coadds").value).map(_.toInt)

  def period: Option[Double] = Option(dcStatus.getDoubleAttribute("period").value).map(_.toDouble)

  def countDown: Option[Double] = Option(dcStatus.getDoubleAttribute("countdown").value)
    .map(_.toDouble)

  def numberOfPeriods: Option[Int] = Option(dcStatus.getIntegerAttribute("numberOfPeriods").value)
    .map(_.toInt)

  def timeMode: Option[String] = Option(dcStatus.getStringAttribute("timeMode").value)

  val dhsConnectedAttr: CaAttribute[DhsConnected] = dcStatus.addEnum[DhsConnected]("dhsConnected",
    s"${NifsTop}sad:dc:dhsConnO", classOf[DhsConnected])

  def dcName: Option[String] = Option(dcStatus.getStringAttribute("name").value)

  def exposureMode: Option[String] = Option(dcStatus.getStringAttribute("expMode").value)

  def readTime: Option[Double] = Option(dcStatus.getDoubleAttribute("readTime").value)
    .map(_.toDouble)

  def biasPwr: Option[Double] = Option(dcStatus.getDoubleAttribute("biasPwr").value)
    .map(_.toDouble)

  val ccStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::status")

  def centralWavelength: Option[Double] =
    Option(ccStatus.getDoubleAttribute("centralWavelength").value).map(_.toDouble)

  def disperser: Option[String] = Option(ccStatus.getStringAttribute("disperser").value)

  def imagingMirror: Option[String] = Option(ccStatus.getStringAttribute("imagingMirror").value)

  def mask: Option[String] = Option(ccStatus.getStringAttribute("mask").value)

  def lastSelectedDisperser: Option[String] = Option(ccStatus.getStringAttribute("lastSelDisp")
    .value)

  def lastSelectedMask: Option[String] = Option(ccStatus.getStringAttribute("lastSelMask").value)

  def maskOffset: Option[Double] = Option(ccStatus.getDoubleAttribute("maskOffset").value)
    .map(_.toDouble)

  def filter: Option[String] = Option(ccStatus.getStringAttribute("filter").value)

  def windowCover: Option[String] = Option(ccStatus.getStringAttribute("windowCover").value)

  def flipName: Option[String] = Option(ccStatus.getStringAttribute("flipName").value)

}

object NifsEpics extends EpicsSystem[NifsEpics] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Nifs.xml"

  override def build(service: CaService, tops: Map[String, String]) = new NifsEpics(service, tops)

}
