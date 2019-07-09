// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.implicits._
import cats.effect.IO
import cats.effect.Sync
import cats.data.Nested
import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.gnirs.{DetectorState => JDetectorState}
import java.lang.{Double => JDouble}
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.EpicsCommand.setParameter
import seqexec.server.EpicsUtil.safeAttribute
import seqexec.server.EpicsUtil.safeAttributeSDouble
import seqexec.server.EpicsUtil.safeAttributeSInt
import seqexec.server.{EpicsSystem, ObserveCommand}
import seqexec.server.EpicsCommand

class GnirsEpics[F[_]: Sync](epicsService: CaService, tops: Map[String, String]) {

  val GnirsTop: String = tops.getOrElse("nirs", "nirs:")

  object configCCCmd extends EpicsCommand {
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

  object configDCCmd extends EpicsCommand {
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

  object endObserveCmd extends EpicsCommand {
    override protected val cs:Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::endObserve"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "nirs::observeCmd", s"${GnirsTop}dc:apply", s"${GnirsTop}dc:applyC", s"${GnirsTop}dc:observeC",
    true, s"${GnirsTop}dc:stop", s"${GnirsTop}dc:abort", ""))

  object stopCmd extends EpicsCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("nirs::abort"))

  object abortCmd extends EpicsCommand {
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

  def arrayId: F[Option[String]] = safeAttribute(dcState.getStringAttribute("arrayid"))

  def arrayType: F[Option[String]] = safeAttribute(dcState.getStringAttribute("arraytyp"))

  def obsEpoch: F[Option[Double]] = Nested(safeAttributeSDouble(dcState.getDoubleAttribute("OBSEPOCH"))).map(_.toDouble).value

  def detBias: F[Option[Double]] = Nested(safeAttributeSDouble(dcState.getDoubleAttribute("detBias"))).map(_.toDouble).value

  def countDown: F[Option[String]] = safeAttribute(dcState.getStringAttribute("countdown"))

  def numCoadds: F[Option[Int]] = Nested(safeAttributeSInt(dcState.getIntegerAttribute("numCoAdds"))).map(_.toInt).value

  def wcs: F[Option[String]] = safeAttribute(dcState.getStringAttribute("wcs"))

  def exposureTime: F[Option[Double]] = Nested(safeAttributeSDouble(dcState.getDoubleAttribute("exposureTime"))).map(_.toDouble).value

  def digitalAvgs: F[Option[Int]] = Nested(safeAttributeSInt(dcState.getIntegerAttribute("digitalAvgs"))).map(_.toInt).value

  def lowNoise: F[Option[Int]] = Nested(safeAttributeSInt(dcState.getIntegerAttribute("lowNoise"))).map(_.toInt).value

  def dhsConnected: F[Option[Boolean]] = Nested(safeAttributeSInt(dcState.getIntegerAttribute("dhsConnected")))
    .map(_.toInt =!= 0).value

  val arrayActiveAttr: Option[CaAttribute[JDetectorState]] = Option(dcState.addEnum(
    "arrayState", s"${GnirsTop}dc:activate", classOf[JDetectorState]
  ))

  def arrayActive: F[Option[Boolean]] =
    arrayActiveAttr
      .map(safeAttribute(_))
      .traverse(r => Nested(r).map(_.getActive).value)
      .map(_.flatten)

  def minInt: F[Option[Double]] = Nested(safeAttributeSDouble(dcState.getDoubleAttribute("minInt"))).map(_.toDouble).value

  def dettemp: F[Option[Double]] = Nested(safeAttributeSDouble(dcState.getDoubleAttribute("dettemp"))).map(_.toDouble).value

  def prism: F[Option[String]] = safeAttribute(state.getStringAttribute("prism"))

  def focus: F[Option[String]] = safeAttribute(state.getStringAttribute("focus"))

  def slitWidth: F[Option[String]] = safeAttribute(state.getStringAttribute("slitWidth"))

  def acqMirror: F[Option[String]] = safeAttribute(state.getStringAttribute("acqMirror"))

  def cover: F[Option[String]] = safeAttribute(state.getStringAttribute("cover"))

  def grating: F[Option[String]] = safeAttribute(state.getStringAttribute("grating"))

  def gratingMode: F[Option[String]] = safeAttribute(state.getStringAttribute("gratingMode"))

  def filter1: F[Option[String]] = safeAttribute(state.getStringAttribute("filter1"))

  def filter2: F[Option[String]] = safeAttribute(state.getStringAttribute("filter2"))

  def camera: F[Option[String]] = safeAttribute(state.getStringAttribute("camera"))

  def decker: F[Option[String]] = safeAttribute(state.getStringAttribute("decker"))

  def centralWavelength: F[Option[Double]] = Nested(safeAttributeSDouble(state.getDoubleAttribute("centralWavelength"))).map(_.toDouble).value

  def gratingTilt: F[Option[Double]] = Nested(safeAttributeSDouble(state.getDoubleAttribute("grattilt"))).map(_.toDouble).value

  def nirscc: F[Option[String]] = safeAttribute(state.getStringAttribute("nirscc"))

  def gratingOrder: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("gratord"))).map(_.toInt).value

  def filter1Eng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("fw1_eng"))).map(_.toInt).value

  def filter2Eng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("fw2_eng"))).map(_.toInt).value

  def deckerEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("dkr_eng"))).map(_.toInt).value

  def gratingEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("gr_eng"))).map(_.toInt).value

  def prismEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("prsm_eng"))).map(_.toInt).value

  def cameraEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("cam_eng"))).map(_.toInt).value

  def slitEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("slit_eng"))).map(_.toInt).value

  def focusEng: F[Option[Int]] = Nested(safeAttributeSInt(state.getIntegerAttribute("fcs_eng"))).map(_.toInt).value

}

object GnirsEpics extends EpicsSystem[GnirsEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Gnirs.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GnirsEpics(service, tops)

}
