// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.effect.{Async, IO, Sync}
import cats.implicits._
import edu.gemini.epics.acm.{CaApplySender, CaAttribute, CaCommandSender, CaParameter, CaService, CaStatusAcceptor, CaWindowStabilizer, CarState}
import edu.gemini.seqexec.server.gsaoi.DhsConnected
import seqexec.server.{EpicsCommand, EpicsCommandBase, EpicsSystem, EpicsUtil, ObserveCommand}
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil.{safeAttributeF, safeAttributeSDoubleF, safeAttributeSIntF}
import java.lang.{Double => JDouble}
import java.util.concurrent.TimeUnit.SECONDS

import scala.concurrent.duration.FiniteDuration

class GsaoiEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  private val GsaoiTop = tops.getOrElse("gsaoi", "gsaoi:")

  object dcConfigCmd extends EpicsCommandBase[F] {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::dcconfig"))

    val fowlerSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfFowSamples"))
    def setFowlerSamples(v: Int): F[Unit] = setParameter(fowlerSamples, Integer.valueOf(v))

    val readMode: Option[CaParameter[String]] = cs.map(_.getString("readMode"))
    def setReadMode(v: String): F[Unit] = setParameter(readMode, v)

    val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    val roi: Option[CaParameter[String]] = cs.map(_.getString("roi"))
    def setRoi(v: String): F[Unit] = setParameter(roi, v)

    val resetDelay: Option[CaParameter[JDouble]] = cs.map(_.getDouble("resetDelay"))
    def setResetDelay(v: Double): F[Unit] = setParameter(resetDelay, JDouble.valueOf(v))

    val numberOfResets: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfResets"))
    def setNumberOfResets(v: Int): F[Unit] = setParameter(numberOfResets, Integer.valueOf(v))

    val coadds: Option[CaParameter[Integer]] = cs.map(_.getInteger("coadds"))
    def setNumberOfCoadds(v: Int): F[Unit] = setParameter(coadds, Integer.valueOf(v))

    val referenceSamples: Option[CaParameter[Integer]] = cs.map(_.getInteger("numberOfRefSamples"))
    def setReferenceSamples(v: Int): F[Unit] = setParameter(referenceSamples, Integer.valueOf(v))

  }

  object ccConfigCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::config"))

    val filter: Option[CaParameter[String]] = cs.map(_.getString("filter"))
    def setFilter(v: String): F[Unit] = setParameter(filter, v)

    val utilWheel: Option[CaParameter[String]] = cs.map(_.getString("utilWheel"))
    def setUtilWheel(v: String): F[Unit] = setParameter(utilWheel, v)

    val windowCover: Option[CaParameter[String]] = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): F[Unit] = setParameter(windowCover, v)

  }

  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "gsaoi::observeCmd", s"${GsaoiTop}dc:stateApply",s"${GsaoiTop}dc:observeC",
    false, s"${GsaoiTop}dc:stop", s"${GsaoiTop}dc:abort", ""))

  object stopCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::stop"))
  }

  object abortCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::abort"))
  }

  object observeCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gsaoi::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    private val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  object endObserveCmd extends EpicsCommandBase[F]{
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("gsaoi::endObserve"))
  }

  val guideApply: CaApplySender = epicsService.createContinuousCommandSender("gsaoi::guideApply",
    s"${GsaoiTop}dc:stateApply",s"${GsaoiTop}dc:guideC", false, "guide start apply")
  object guideCmd extends EpicsCommandBase[F]{
    override val cs: Option[CaCommandSender] =
      Option(epicsService.createCommandSender("gsaoi:guide", guideApply, s"${GsaoiTop}dc:guide"))
  }

  val endGuideCmd: EpicsCommand[F] = new EpicsCommandBase[F] {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("gsaoi::endGuide"))
  }

  val status: CaStatusAcceptor = epicsService.getStatusAcceptor("gsaoi::status")

  def windowCover: F[String] = safeAttributeF(status.getStringAttribute("windowCover"))

  def lowerFilter: F[String] = safeAttributeF(status.getStringAttribute("lowerFilter"))

  def filter: F[String] = safeAttributeF(status.getStringAttribute("filter"))

  def upperFilter: F[String] = safeAttributeF(status.getStringAttribute("upperFilter"))

  def utilWheel: F[String] = safeAttributeF(status.getStringAttribute("utilWheel"))

  def dspCodeVersion: F[String] =  safeAttributeF(status.getStringAttribute("DSPTIMBV"))

  def coldworkSurfaceTemperature: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("CWSTEMP"))

  def bUnits: F[String] = safeAttributeF(status.getStringAttribute("BUNITS"))

  def windowCoverEngPos: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("CVERPOS"))

  def dcHealth: F[String] = safeAttributeF(status.getStringAttribute("DCHLTH"))

  def lowerFilterHealth: F[String] = safeAttributeF(status.getStringAttribute("FILT2CAR"))

  def simulationMode: F[String] = safeAttributeF(status.getStringAttribute("DCSIM"))

  def timingBoardCodeName: F[String] = safeAttributeF(status.getStringAttribute("DSPTIMBN"))

  def readInterval: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("READDLAY"))

  def detectorTemperature: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("DETTEMP"))

  def upperFilterHealth: F[String] = safeAttributeF(status.getStringAttribute("FILT1CAR"))

  def dewarPressure: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("DEWPRES"))

  def obsElapsedTime: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("ELAPSED"))

  def lowerFilterEngPos: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("FILT2POS"))

  def resetDelay: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("RSTDLAY"))

  def detectorHousingTemperature: F[Double] =
    safeAttributeSDoubleF(status.getDoubleAttribute("DETHTEMP"))

  def utilityWheelEngPos: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("UTLWPOS"))

  def utilityWheelHealth: F[String] = safeAttributeF(status.getStringAttribute("UTLWCAR"))

  def windowCoverHealth: F[String] = safeAttributeF(status.getStringAttribute("CVERCAR"))

  def upperFilterEngPos: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("FILT1POS"))

  def expositionMode: F[String] = safeAttributeF(status.getStringAttribute("EXPMODE"))

  def dcName: F[String] = safeAttributeF(status.getStringAttribute("DCNAME"))

  def pciBoardCodeName: F[String] = safeAttributeF(status.getStringAttribute("DSPPCIN"))

  def readTime: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("READTIME"))

  def requestedExposureTime: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("exposureTime"))

  def numberOfResets: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("numberOfResets"))

  def readMode: F[String] = safeAttributeF(status.getStringAttribute("readMode"))

  def numberOfFowlerSamples: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("numberOfFowSamples"))

  def coadds: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("coadds"))

  def exposedTime: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("exposedTime"))

  def timeMode: F[String] = safeAttributeF(status.getStringAttribute("timeMode"))

  def roi: F[String] = safeAttributeF(status.getStringAttribute("roi"))

  def countdown: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("countdown"))

  def coaddsDone: F[Int] =  safeAttributeSIntF(status.getIntegerAttribute("coaddsDone"))

  def mjdobs: F[Double] =  safeAttributeSDoubleF(status.getDoubleAttribute("mjdobs"))

  private val dhsConnectedAttr: CaAttribute[DhsConnected] =
    status.addEnum[DhsConnected]("dhsConnected", s"${GsaoiTop}sad:dc:dhsConnO", classOf[DhsConnected])
  def dhsConnected: F[DhsConnected] = safeAttributeF(dhsConnectedAttr)

  private val observeCAttr: CaAttribute[CarState] = status.addEnum("observeC",
    s"${GsaoiTop}dc:observeC.VAL", classOf[CarState])
  def observeState: F[CarState] = safeAttributeF(observeCAttr)

  private val notGuidingAttr = status.getIntegerAttribute("notGuiding")
  def guiding: F[Boolean] = safeAttributeSIntF(notGuidingAttr).map(_ === 0)

  private val guideStabilizeTime = FiniteDuration(1, SECONDS)
  private val filteredNotGuidingAttr: CaWindowStabilizer[Integer] =
    new CaWindowStabilizer[Integer](notGuidingAttr, java.time.Duration.ofMillis(guideStabilizeTime.toMillis))

  private val guideTimeout = FiniteDuration(5, SECONDS)
  def waitForGuideOn: F[Unit] =
    Async[F].delay(filteredNotGuidingAttr.restart)
      .flatMap(EpicsUtil.waitForValueF[Integer, F](_, 0, guideTimeout, "ODGW guide flag"))
  def waitForGuideOff: F[Unit] =
    Async[F].delay(filteredNotGuidingAttr.restart)
      .flatMap(EpicsUtil.waitForValueF[Integer, F](_, 1, guideTimeout, "ODGW guide flag"))

  def odgwBaseExpTime: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("baseExpTime"))

  def odgw1Counts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("counts1"))

  def odgw2Counts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("counts2"))

  def odgw3Counts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("counts3"))

  def odgw4Counts: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("counts4"))

  def odgw1Multiplier: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("expMult1"))

  def odgw2Multiplier: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("expMult2"))

  def odgw3Multiplier: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("expMult3"))

  def odgw4Multiplier: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("expMult4"))

  def odgwSize: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgwSize"))

  def odgw1X: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw1x"))

  def odgw1Y: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw1y"))

  def odgw2X: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw2x"))

  def odgw2Y: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw2y"))

  def odgw3X: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw3x"))

  def odgw3Y: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw3y"))

  def odgw4X: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw4x"))

  def odgw4Y: F[Int] = safeAttributeSIntF(status.getIntegerAttribute("odgw4y"))

  def dcIsPreparing: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("notPrepObs")).map(_ === 0)

  def dcIsAcquiring: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("notAcqObs")).map(_ === 0)

  def dcIsReadingOut: F[Boolean] = safeAttributeSIntF(status.getIntegerAttribute("notReadingOut")).map(_ === 0)

}

object GsaoiEpics extends EpicsSystem[GsaoiEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gsaoi.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GsaoiEpics[IO]] =
    Sync[F].delay(new GsaoiEpics[IO](service, tops))

}
