// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.effect.{Async, IO, Sync}
import cats.implicits._
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.nifs.DhsConnected
import edu.gemini.seqexec.server.nifs.ReadMode
import edu.gemini.seqexec.server.nifs.TimeMode
import java.lang.{Double => JDouble}

import seqexec.server.EpicsSystem
import seqexec.server.EpicsCommandBase
import seqexec.server.ObserveCommand
import seqexec.server.EpicsUtil.safeAttributeF
import seqexec.server.EpicsUtil.safeAttributeSDoubleF
import seqexec.server.EpicsUtil.safeAttributeSIntF
import seqexec.server.EpicsCommandBase.setParameter

class NifsEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {
  val NifsTop = tops.getOrElse("nifs", "nifs:")

  object ccConfigCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nifs::config"))

    val disperser: Option[CaParameter[String]] = cs.map(_.getString("disperser"))
    def setDisperser(v: String): F[Unit] = setParameter(disperser, v)

    val filter: Option[CaParameter[String]] = cs.map(_.getString("filter"))
    def setFilter(v: String): F[Unit] = setParameter(filter, v)

    val windowCover: Option[CaParameter[String]] = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): F[Unit] = setParameter(windowCover, v)

    val maskOffset: Option[CaParameter[JDouble]] = cs.map(_.getDouble("maskOffset"))
    def setMaskOffset(v: Double): F[Unit] = setParameter(maskOffset, JDouble.valueOf(v))

    val imagingMirror: Option[CaParameter[String]] = cs.map(_.getString("imagingMirror"))
    def setImagingMirror(v: String): F[Unit] = setParameter(imagingMirror, v)

    val mask: Option[CaParameter[String]] = cs.map(_.getString("mask"))
    def setMask(v: String): F[Unit] = setParameter(mask, v)

    val centralWavelength: Option[CaParameter[JDouble]] = cs.map(_.getDouble("centralWavelength"))
    def setCentralWavelength(v: Double): F[Unit] = setParameter(centralWavelength, JDouble.valueOf(v))

  }

  object dcConfigCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nifs::dcconfig"))

    private val coadds: Option[CaParameter[Integer]] = cs.map(_.getInteger("coadds"))
    def setCoadds(v: Int): F[Unit] = setParameter(coadds, Integer.valueOf(v))

    private val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    private val fowlerSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfFowSamples"))
    def setFowlerSamples(v: Int): F[Unit] = setParameter(fowlerSamples, Integer.valueOf(v))

    private val period: Option[CaParameter[JDouble]] = cs.map(_.getDouble("period"))
    def setPeriod(v: Double): F[Unit] = setParameter(period, JDouble.valueOf(v))

    private val readMode: Option[CaParameter[ReadMode]] = cs.map(c =>
      c.addEnum[ReadMode]("readMode", s"${NifsTop}dc:obs_readMode", classOf[ReadMode], false)
    )
    def setReadMode(v: ReadMode): F[Unit] = setParameter(readMode, v)

    private val numberOfResets: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfResets"))
    def setnumberOfResets(v: Int): F[Unit] =
      setParameter(numberOfResets, Integer.valueOf(v))

    private val numberOfPeriods: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfPeriods"))
    def setnumberOfPeriods(v: Int): F[Unit] =
      setParameter(numberOfPeriods, Integer.valueOf(v))

    private val timeMode: Option[CaParameter[TimeMode]] = cs.map(c =>
      c.addEnum[TimeMode]("timeMode", s"${NifsTop}dc:obs_timeMode", classOf[TimeMode], false)
    )
    def setTimeMode(v: TimeMode): F[Unit] =
      setParameter(timeMode, v)
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nifs::stop"))

  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "nifs::observeCmd", s"${NifsTop}dc:nifsApply", s"${NifsTop}dc:applyC", s"${NifsTop}dc:observeC",
    false, s"${NifsTop}dc:stop", s"${NifsTop}dc:abort", ""))

  object stopCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nifs::abort"))

  object abortCmd extends EpicsCommandBase[F]{
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

    private val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  object endObserveCmd extends EpicsCommandBase[F]{
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("nifs::endObserve"))
  }

  private val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::dcstatus")

  def exposureTime: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("exposureTime"))

  def exposedTime: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("exposedTime"))

  def numberOfResets: F[Int] =
    safeAttributeSIntF(dcStatus.getIntegerAttribute("numberOfResets"))

  def readMode: F[String] =
    safeAttributeF(dcStatus.getStringAttribute("readMode"))

  def numberOfFowSamples: F[Int] =
    safeAttributeSIntF(dcStatus.getIntegerAttribute("numberOfFowSamples"))

  def coadds: F[Int] =
    safeAttributeSIntF(dcStatus.getIntegerAttribute("coadds"))

  def period: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("period"))

  def countDown: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("countdown"))

  def numberOfPeriods: F[Int] =
    safeAttributeSIntF(dcStatus.getIntegerAttribute("numberOfPeriods"))

  def timeMode: F[String] =
    safeAttributeF(dcStatus.getStringAttribute("timeMode"))

  private val dhsConnectedAttr: CaAttribute[DhsConnected] =
    dcStatus.addEnum[DhsConnected]("dhsConnected", s"${NifsTop}sad:dc:dhsConnO", classOf[DhsConnected])

  def dhsConnected: F[DhsConnected] =
    safeAttributeF(dhsConnectedAttr)

  def dcName: F[String] =
    safeAttributeF(dcStatus.getStringAttribute("name"))

  def exposureMode: F[String] =
    safeAttributeF(dcStatus.getStringAttribute("expMode"))

  def readTime: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("readTime"))

  def biasPwr: F[Double] =
    safeAttributeSDoubleF(dcStatus.getDoubleAttribute("biasPwr"))

  private val ccStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("nifs::status")

  def centralWavelength: F[Double] =
    safeAttributeSDoubleF(ccStatus.getDoubleAttribute("centralWavelength"))

  def disperser: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("disperser"))

  def imagingMirror: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("imagingMirror"))

  def mask: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("mask"))

  def lastSelectedDisperser: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("lastSelDisp"))

  def lastSelectedMask: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("lastSelMask"))

  def maskOffset: F[Double] =
    safeAttributeSDoubleF(ccStatus.getDoubleAttribute("maskOffset"))

  def filter: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("filter"))

  def windowCover: F[String] =
    safeAttributeF(ccStatus.getStringAttribute("windowCover"))

  def dcIsPreparing: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("notPrepObs")).map(_ === 0)

  def dcIsAcquiring: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("notAcqObs")).map(_ === 0)

  def dcIsReadingOut: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("notReadingOut")).map(_ === 0)

}

object NifsEpics extends EpicsSystem[NifsEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Nifs.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[NifsEpics[IO]] =
    Sync[F].delay(new NifsEpics[IO](service, tops))

}
