// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.effect.{Async, IO}
import cats.implicits._
import edu.gemini.epics.acm.{CaApplySender, CaAttribute, CaCommandSender, CaParameter, CaService, CaStatusAcceptor, CaWindowStabilizer, CarState}
import edu.gemini.seqexec.server.gsaoi.DhsConnected
import seqexec.server.{EpicsCommandF, EpicsSystem, EpicsUtil, ObserveCommandF}
import seqexec.server.EpicsCommand.setParameterF
import seqexec.server.EpicsUtil.{safeAttribute, safeAttributeSDouble, safeAttributeSInt}
import java.lang.{Double => JDouble}

import squants.time.TimeConversions._
import cats.data.Nested
import org.log4s.{Logger, getLogger}

class GsaoiEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  private val GsaoiTop = tops.getOrElse("gsaoi", "gsaoi:")

  object dcConfigCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::dcconfig"))

    val fowlerSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfFowSamples"))
    def setFowlerSamples(v: Int): F[Unit] = setParameterF(fowlerSamples, Integer.valueOf(v))

    val readMode: Option[CaParameter[String]] = cs.map(_.getString("readMode"))
    def setReadMode(v: String): F[Unit] = setParameterF(readMode, v)

    val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameterF(exposureTime, JDouble.valueOf(v))

    val roi: Option[CaParameter[String]] = cs.map(_.getString("roi"))
    def setRoi(v: String): F[Unit] = setParameterF(roi, v)

    val resetDelay: Option[CaParameter[JDouble]] = cs.map(_.getDouble("resetDelay"))
    def setResetDelay(v: Double): F[Unit] = setParameterF(resetDelay, JDouble.valueOf(v))

    val numberOfResets: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfResets"))
    def setNumberOfResets(v: Int): F[Unit] = setParameterF(numberOfResets, Integer.valueOf(v))

    val coadds: Option[CaParameter[Integer]] = cs.map(_.getInteger("coadds"))
    def setNumberOfCoadds(v: Int): F[Unit] = setParameterF(coadds, Integer.valueOf(v))

    val referenceSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfRefSamples"))
    def setReferenceSamples(v: Int): F[Unit] = setParameterF(referenceSamples, Integer.valueOf(v))

  }

  object ccConfigCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::config"))

    val filter: Option[CaParameter[String]] = cs.map(_.getString("filter"))
    def setFilter(v: String): F[Unit] = setParameterF(filter, v)

    val utilWheel: Option[CaParameter[String]] = cs.map(_.getString("utilWheel"))
    def setUtilWheel(v: String): F[Unit] = setParameterF(utilWheel, v)

    val windowCover: Option[CaParameter[String]] = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): F[Unit] = setParameterF(windowCover, v)

  }

  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "gsaoi::observeCmd", s"${GsaoiTop}dc:stateApply",s"${GsaoiTop}dc:observeC",
    false, s"${GsaoiTop}dc:stop", s"${GsaoiTop}dc:abort", ""))

  object stopCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::stop"))
  }

  object abortCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::abort"))
  }

  object observeCmd extends ObserveCommandF {
    override protected val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gsaoi::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    private val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameterF(label, v)
  }

  object endObserveCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gsaoi::endObserve"))
  }

  val guideApply: CaApplySender = epicsService.createContinuousCommandSender("gsaoi::guideApply",
    s"${GsaoiTop}dc:stateApply",s"${GsaoiTop}dc:guideC", false, "guide start apply")
  object guideCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] =
      Option(epicsService.createCommandSender("gsaoi:guide", guideApply, s"${GsaoiTop}dc:guide"))
  }

  val endGuideCmd: EpicsCommandF = new EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::endGuide"))
  }

  val status: CaStatusAcceptor = epicsService.getStatusAcceptor("gsaoi::status")

  def windowCover: F[Option[String]] = safeAttribute(status.getStringAttribute("windowCover"))

  def lowerFilter: F[Option[String]] = safeAttribute(status.getStringAttribute("lowerFilter"))

  def filter: F[Option[String]] = safeAttribute(status.getStringAttribute("filter"))

  def upperFilter: F[Option[String]] = safeAttribute(status.getStringAttribute("upperFilter"))

  def utilWheel: F[Option[String]] = safeAttribute(status.getStringAttribute("utilWheel"))

  def dspCodeVersion: F[Option[String]] =  safeAttribute(status.getStringAttribute("DSPTIMBV"))

  def coldworkSurfaceTemperature: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("CWSTEMP"))

  def bUnits: F[Option[String]] = safeAttribute(status.getStringAttribute("BUNITS"))

  def windowCoverEngPos: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("CVERPOS"))

  def dcHealth: F[Option[String]] = safeAttribute(status.getStringAttribute("DCHLTH"))

  def lowerFilterHealth: F[Option[String]] = safeAttribute(status.getStringAttribute("FILT2CAR"))

  def simulationMode: F[Option[String]] = safeAttribute(status.getStringAttribute("DCSIM"))

  def timingBoardCodeName: F[Option[String]] = safeAttribute(status.getStringAttribute("DSPTIMBN"))

  def readInterval: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("READDLAY"))

  def detectorTemperature: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("DETTEMP"))

  def upperFilterHealth: F[Option[String]] = safeAttribute(status.getStringAttribute("FILT1CAR"))

  def dewarPressure: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("DEWPRES"))

  def obsElapsedTime: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("ELAPSED"))

  def lowerFilterEngPos: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("FILT2POS"))

  def resetDelay: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("RSTDLAY"))

  def detectorHousingTemperature: F[Option[Double]] =
    safeAttributeSDouble(status.getDoubleAttribute("DETHTEMP"))

  def utilityWheelEngPos: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("UTLWPOS"))

  def utilityWheelHealth: F[Option[String]] = safeAttribute(status.getStringAttribute("UTLWCAR"))

  def windowCoverHealth: F[Option[String]] = safeAttribute(status.getStringAttribute("CVERCAR"))

  def upperFilterEngPos: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("FILT1POS"))

  def expositionMode: F[Option[String]] = safeAttribute(status.getStringAttribute("EXPMODE"))

  def dcName: F[Option[String]] = safeAttribute(status.getStringAttribute("DCNAME"))

  def pciBoardCodeName: F[Option[String]] = safeAttribute(status.getStringAttribute("DSPPCIN"))

  def readTime: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("READTIME"))

  def requestedExposureTime: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("exposureTime"))

  def numberOfResets: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("numberOfResets"))

  def readMode: F[Option[String]] = safeAttribute(status.getStringAttribute("readMode"))

  def numberOfFowlerSamples: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("numberOfFowSamples"))

  def coadds: F[Option[Int]] = safeAttributeSInt(status.getIntegerAttribute("coadds"))

  def exposedTime: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("exposedTime"))

  def timeMode: F[Option[String]] = safeAttribute(status.getStringAttribute("timeMode"))

  def roi: F[Option[String]] = safeAttribute(status.getStringAttribute("roi"))

  def countdown: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("countdown"))

  def coaddsDone: F[Option[Int]] =  safeAttributeSInt(status.getIntegerAttribute("coaddsDone"))

  def mjdobs: F[Option[Double]] =  safeAttributeSDouble(status.getDoubleAttribute("mjdobs"))

  private val dhsConnectedAttr: CaAttribute[DhsConnected] =
    status.addEnum[DhsConnected]("dhsConnected", s"${GsaoiTop}sad:dc:dhsConnO", classOf[DhsConnected])
  def dhsConnected: F[Option[DhsConnected]] = safeAttribute(dhsConnectedAttr)

  private val observeCAttr: CaAttribute[CarState] = status.addEnum("observeC",
    s"${GsaoiTop}dc:observeC.VAL", classOf[CarState])
  def observeState: F[Option[CarState]] = safeAttribute(observeCAttr)

  private val notGuidingAttr = status.getIntegerAttribute("notGuiding")
  def guiding: F[Option[Boolean]] = Nested(safeAttributeSInt(notGuidingAttr)).map(_ === 0).value

  private val guideStabilizeTime = 1.seconds
  private val filteredNotGuidingAttr: CaWindowStabilizer[Integer] =
    new CaWindowStabilizer[Integer](notGuidingAttr, java.time.Duration.ofMillis(guideStabilizeTime.toMillis))

  private val guideTimeout = 5.seconds
  def waitForGuideOn: F[Unit] =
    Async[F].delay(filteredNotGuidingAttr.reset)
      .flatMap(EpicsUtil.waitForValueF[Integer, F](_, 0, guideTimeout, "ODGW guide flag"))
  def waitForGuideOff: F[Unit] =
    Async[F].delay(filteredNotGuidingAttr.reset)
      .flatMap(EpicsUtil.waitForValueF[Integer, F](_, 1, guideTimeout, "ODGW guide flag"))

}

object GsaoiEpics extends EpicsSystem[GsaoiEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Gsaoi.xml"

  override def build(service: CaService, tops: Map[String, String]) =
    new GsaoiEpics[IO](service, tops)

}
