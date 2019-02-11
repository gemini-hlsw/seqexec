// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.IO
import cats.effect.Sync
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.nifs.DhsConnected
import java.lang.{Double => JDouble}
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.{EpicsCommand, EpicsSystem, ObserveCommand, SeqAction}
import seqexec.server.EpicsUtil.safeAttribute
import seqexec.server.EpicsUtil.safeAttributeSDouble
import seqexec.server.EpicsUtil.safeAttributeSInt

class NifsEpics[F[_]: Sync](epicsService: CaService, tops: Map[String, String]) {
  val NifsTop = tops.getOrElse("nifs", "nifs:")

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

  private val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::dcstatus")

  def exposureTime: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("exposureTime"))

  def exposedTime: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("exposedTime"))

  def numberOfResets: F[Option[Int]] =
    safeAttributeSInt(dcStatus.getIntegerAttribute("numberOfResets"))

  def readMode: F[Option[String]] =
    safeAttribute(dcStatus.getStringAttribute("readMode"))

  def numberOfFowSamples: F[Option[Int]] =
    safeAttributeSInt(dcStatus.getIntegerAttribute("numberOfFowSamples"))

  def coadds: F[Option[Int]] =
    safeAttributeSInt(dcStatus.getIntegerAttribute("coadds"))

  def period: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("period"))

  def countDown: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("countdown"))

  def numberOfPeriods: F[Option[Int]] =
    safeAttributeSInt(dcStatus.getIntegerAttribute("numberOfPeriods"))

  def timeMode: F[Option[String]] =
    safeAttribute(dcStatus.getStringAttribute("timeMode"))

  val dhsConnectedAttr: CaAttribute[DhsConnected] = dcStatus.addEnum[DhsConnected]("dhsConnected",
    s"${NifsTop}sad:dc:dhsConnO", classOf[DhsConnected])

  def dcName: F[Option[String]] =
    safeAttribute(dcStatus.getStringAttribute("name"))

  def exposureMode: F[Option[String]] =
    safeAttribute(dcStatus.getStringAttribute("expMode"))

  def readTime: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("readTime"))

  def biasPwr: F[Option[Double]] =
    safeAttributeSDouble(dcStatus.getDoubleAttribute("biasPwr"))

  private val ccStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::status")

  def centralWavelength: F[Option[Double]] =
    safeAttributeSDouble(ccStatus.getDoubleAttribute("centralWavelength"))

  def disperser: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("disperser"))

  def imagingMirror: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("imagingMirror"))

  def mask: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("mask"))

  def lastSelectedDisperser: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("lastSelDisp"))

  def lastSelectedMask: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("lastSelMask"))

  def maskOffset: F[Option[Double]] =
    safeAttributeSDouble(ccStatus.getDoubleAttribute("maskOffset"))

  def filter: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("filter"))

  def windowCover: F[Option[String]] =
    safeAttribute(ccStatus.getStringAttribute("windowCover"))

}

object NifsEpics extends EpicsSystem[NifsEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Nifs.xml"

  override def build(service: CaService, tops: Map[String, String]) =
    new NifsEpics[IO](service, tops)

}
