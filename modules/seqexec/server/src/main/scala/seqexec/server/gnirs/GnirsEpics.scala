// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.implicits._
import cats.effect.{Async, IO, Sync}
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.gnirs.{DetectorState => JDetectorState}
import java.lang.{Double => JDouble}

import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil.safeAttributeF
import seqexec.server.EpicsUtil.safeAttributeSDoubleF
import seqexec.server.EpicsUtil.safeAttributeSIntF
import seqexec.server.{EpicsSystem, ObserveCommand}
import seqexec.server.EpicsCommandBase

class GnirsEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  val GnirsTop: String = tops.getOrElse("nirs", "nirs:")

  object configCCCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::config"))

    private val cover: Option[CaParameter[String]] = cs.map(_.getString("cover"))
    def setCover(v: String): F[Unit] = setParameter(cover, v)

    private val filter1: Option[CaParameter[String]] = cs.map(_.getString("filter1"))
    def setFilter1(v: String): F[Unit] = setParameter(filter1, v)

    private val filter2: Option[CaParameter[String]] = cs.map(_.getString("filter2"))
    def setFilter2(v: String): F[Unit] = setParameter(filter2, v)

    private val focus: Option[CaParameter[Integer]] = cs.map(_.getInteger("focus"))
    def setFocus(v: Int): F[Unit] = setParameter(focus, Integer.valueOf(v))

    private val tilt: Option[CaParameter[String]] = cs.map(_.getString("tilt"))
    def setTilt(v: String): F[Unit] = setParameter(tilt, v)

    private val prism: Option[CaParameter[String]] = cs.map(_.getString("prism"))
    def setPrism(v: String): F[Unit] = setParameter(prism, v)

    private val acqMirror: Option[CaParameter[String]] = cs.map(_.getString("acqMirror"))
    def setAcqMirror(v: String): F[Unit] = setParameter(acqMirror, v)

    private val focusbest: Option[CaParameter[String]] = cs.map(_.getString("focusbest"))
    def setFocusBest(v: String): F[Unit] = setParameter(focusbest, v)

    private val centralWavelength: Option[CaParameter[JDouble]] = cs.map(_.getDouble("centralWavelength"))
    def setCentralWavelength(v: Double): F[Unit] = setParameter(centralWavelength, JDouble.valueOf(v))

    private val camera: Option[CaParameter[String]] = cs.map(_.getString("camera"))
    def setCamera(v: String): F[Unit] = setParameter(camera, v)

    private val gratingMode: Option[CaParameter[String]] = cs.map(_.getString("gratingMode"))
    def setGratingMode(v: String): F[Unit] = setParameter(gratingMode, v)

    private val gratingOrder: Option[CaParameter[Integer]] = cs.map(_.getInteger("order"))
    def setOrder(v: Int): F[Unit] = setParameter(gratingOrder, Integer.valueOf(v))

    private val grating: Option[CaParameter[String]] = cs.map(_.getString("grating"))
    def setGrating(v: String): F[Unit] = setParameter(grating, v)

    private val slitWidth: Option[CaParameter[String]] = cs.map(_.getString("slitWidth"))
    def setSlitWidth(v: String): F[Unit] = setParameter(slitWidth, v)

    private val decker: Option[CaParameter[String]] = cs.map(_.getString("decker"))
    def setDecker(v: String): F[Unit] = setParameter(decker, v)

  }

  object configDCCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::dcconfig"))

    private val lowNoise: Option[CaParameter[Integer]] = cs.map(_.getInteger("lowNoise"))
    def setLowNoise(v: Int): F[Unit] = setParameter(lowNoise, Integer.valueOf(v))

    private val exposureTime: Option[CaParameter[JDouble]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    private val wcs: Option[CaParameter[String]] = cs.map(_.getString("wcs"))
    def setWcs(v: String): F[Unit] = setParameter(wcs, v)

    private val digitalAvgs: Option[CaParameter[Integer]] = cs.map(_.getInteger("digitalAvgs"))
    def setDigitalAvgs(v: Int): F[Unit] = setParameter(digitalAvgs, Integer.valueOf(v))

    private val detBias: Option[CaParameter[JDouble]] = cs.map(_.getDouble("detBias"))
    def setDetBias(v: Double): F[Unit] = setParameter(detBias, JDouble.valueOf(v))

    private val coadds: Option[CaParameter[JDouble]] = cs.map(_.getDouble("coadds"))
    def setCoadds(v: Int): F[Unit] = setParameter(coadds, JDouble.valueOf(v.toDouble))

  }

  object endObserveCmd extends EpicsCommandBase {
    override protected val cs:Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::endObserve"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "nirs::observeCmd", s"${GnirsTop}dc:apply", s"${GnirsTop}dc:applyC", s"${GnirsTop}dc:observeC",
    true, s"${GnirsTop}dc:stop", s"${GnirsTop}dc:abort", ""))

  object stopCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::abort"))

  object abortCmd extends EpicsCommandBase {
    override protected val cs: Option[CaCommandSender] = abortCS
  }

  object abortAndWait extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  object observeCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  private val state: CaStatusAcceptor = epicsService.getStatusAcceptor("nirs::status")
  private val dcState: CaStatusAcceptor = epicsService.getStatusAcceptor("nirs::dcstatus")

  def arrayId: F[String] = safeAttributeF(dcState.getStringAttribute("arrayid"))

  def arrayType: F[String] = safeAttributeF(dcState.getStringAttribute("arraytyp"))

  def obsEpoch: F[Double] = safeAttributeSDoubleF(dcState.getDoubleAttribute("OBSEPOCH"))

  def detBias: F[Double] = safeAttributeSDoubleF(dcState.getDoubleAttribute("detBias"))

  def countDown: F[String] = safeAttributeF(dcState.getStringAttribute("countdown"))

  def numCoadds: F[Int] = safeAttributeSIntF(dcState.getIntegerAttribute("numCoAdds"))

  def wcs: F[String] = safeAttributeF(dcState.getStringAttribute("wcs"))

  def exposureTime: F[Double] = safeAttributeSDoubleF(dcState.getDoubleAttribute("exposureTime"))

  def digitalAvgs: F[Int] = safeAttributeSIntF(dcState.getIntegerAttribute("digitalAvgs"))

  def lowNoise: F[Int] = safeAttributeSIntF(dcState.getIntegerAttribute("lowNoise"))

  def dhsConnected: F[Boolean] = safeAttributeSIntF(dcState.getIntegerAttribute("dhsConnected")).map(_ =!= 0)

  val arrayActiveAttr: CaAttribute[JDetectorState] = dcState.addEnum(
    "arrayState", s"${GnirsTop}dc:activate", classOf[JDetectorState]
  )

  def arrayActive: F[Boolean] = safeAttributeF(arrayActiveAttr).map(_.getActive)

  def minInt: F[Double] = safeAttributeSDoubleF(dcState.getDoubleAttribute("minInt"))

  def dettemp: F[Double] = safeAttributeSDoubleF(dcState.getDoubleAttribute("dettemp"))

  def dcIsPreparing: F[Boolean] = safeAttributeSIntF(dcState.getIntegerAttribute("prepObs")).map(_ =!= 0)

  def dcIsAcquiring: F[Boolean] = safeAttributeSIntF(dcState.getIntegerAttribute("acqObs")).map(_ =!= 0)

  def dcIsReadingOut: F[Boolean] = safeAttributeSIntF(dcState.getIntegerAttribute("readingOut")).map(_ =!= 0)

  def prism: F[String] = safeAttributeF(state.getStringAttribute("prism"))

  def focus: F[String] = safeAttributeF(state.getStringAttribute("focus"))

  def slitWidth: F[String] = safeAttributeF(state.getStringAttribute("slitWidth"))

  def acqMirror: F[String] = safeAttributeF(state.getStringAttribute("acqMirror"))

  def cover: F[String] = safeAttributeF(state.getStringAttribute("cover"))

  def grating: F[String] = safeAttributeF(state.getStringAttribute("grating"))

  def gratingMode: F[String] = safeAttributeF(state.getStringAttribute("gratingMode"))

  def filter1: F[String] = safeAttributeF(state.getStringAttribute("filter1"))

  def filter2: F[String] = safeAttributeF(state.getStringAttribute("filter2"))

  def camera: F[String] = safeAttributeF(state.getStringAttribute("camera"))

  def decker: F[String] = safeAttributeF(state.getStringAttribute("decker"))

  def centralWavelength: F[Double] = safeAttributeSDoubleF(state.getDoubleAttribute("centralWavelength"))

  def gratingTilt: F[Double] = safeAttributeSDoubleF(state.getDoubleAttribute("grattilt"))

  def nirscc: F[String] = safeAttributeF(state.getStringAttribute("nirscc"))

  def gratingOrder: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("gratord"))

  def filter1Eng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("fw1_eng"))

  def filter2Eng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("fw2_eng"))

  def deckerEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("dkr_eng"))

  def gratingEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("gr_eng"))

  def prismEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("prsm_eng"))

  def cameraEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("cam_eng"))

  def slitEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("slit_eng"))

  def focusEng: F[Int] = safeAttributeSIntF(state.getIntegerAttribute("fcs_eng"))
}

object GnirsEpics extends EpicsSystem[GnirsEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gnirs.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GnirsEpics[IO]] =
    Sync[F].delay(new GnirsEpics(service, tops))

}
