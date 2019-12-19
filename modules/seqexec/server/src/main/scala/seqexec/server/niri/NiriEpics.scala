// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.{Async, IO, Sync}
import cats.implicits._
import java.lang.{Double => JDouble}

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.niri.{Disperser => JDisperser}
import edu.gemini.seqexec.server.niri.{ReadMode => JReadMode}
import edu.gemini.seqexec.server.niri.{Mask => JMask}
import edu.gemini.seqexec.server.niri.{Camera => JCamera}
import edu.gemini.seqexec.server.niri.{BeamSplitter => JBeamSplitter}
import edu.gemini.seqexec.server.niri.{BuiltInROI => JBuiltInROI}
import edu.gemini.seqexec.server.niri.{DetectorState => JDetectorState}
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.{EpicsCommandBase, EpicsSystem, ObserveCommand}
import seqexec.server.EpicsUtil.{safeAttributeF, safeAttributeSDoubleF, safeAttributeSIntF}

class NiriEpics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  val NiriTop = tops.getOrElse("niri", "niri:")
  val NisTop = tops.getOrElse("nis", "NIS:")

  object configCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nis::config"))

    private val disperser: Option[CaParameter[JDisperser]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("disperser", s"${NisTop}grism:menu", classOf[JDisperser], false)))
    def setDisperser(v: JDisperser): F[Unit] = setParameter(disperser, v)

    private val readMode: Option[CaParameter[JReadMode]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("readmode", s"${NisTop}readmode:menu", classOf[JReadMode], false)))
    def setReadMode(v: JReadMode): F[Unit] = setParameter(readMode, v)

    private val coadds: Option[CaParameter[Integer]] = cs.flatMap(cmd => Option(cmd.getInteger("numCoAdds")))
    def setCoadds(v: Int): F[Unit] = setParameter(coadds, Integer.valueOf(v))

    private val mask: Option[CaParameter[JMask]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("mask", s"${NisTop}fpmask:menu", classOf[JMask], false)))
    def setMask(v: JMask): F[Unit] = setParameter(mask, v)

    private val camera: Option[CaParameter[JCamera]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("camera", s"${NisTop}camera:menu", classOf[JCamera], false)))
    def setCamera(v: JCamera): F[Unit] = setParameter(camera, v)

    private val beamSplitter: Option[CaParameter[JBeamSplitter]] = cs.flatMap(cmd => Option(
      cmd.addEnum("beamSplitter", s"${NisTop}beamsplit:menu", classOf[JBeamSplitter], false)))
    def setBeamSplitter(v: JBeamSplitter): F[Unit] = setParameter(beamSplitter, v)

    private val exposureTime: Option[CaParameter[JDouble]] = cs.flatMap(cmd =>
      Option(cmd.getDouble("exposureTime")))
    def setExposureTime(v: Double): F[Unit] = setParameter(exposureTime, JDouble.valueOf(v))

    private val builtInROI: Option[CaParameter[JBuiltInROI]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("builtinROI", s"${NisTop}roi:menu", classOf[JBuiltInROI], false)))
    def setBuiltInROI(v: JBuiltInROI): F[Unit] = setParameter(builtInROI, v)

    private val filter: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString("filter")))
    def setFilter(v: String): F[Unit] = setParameter(filter, v)

    private val focus: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString
    ("focus")))
    def setFocus(v: String): F[Unit] = setParameter(focus, v)

  }

  /*
   * For some reason the window cover is not include in the IS configuration parameters. It is
   * applied by the IS apply command, nevertheless. This command exists only to set the parameter.
   */
  object windowCoverConfig extends EpicsCommandBase[F]{
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri:config"))

    private val windowCover: Option[CaParameter[String]] = cs.flatMap(cmd =>
      Option(cmd.getString("windowCover")))
    def setWindowCover(v: String): F[Unit] = setParameter(windowCover, v)
  }

  object endObserveCmd extends EpicsCommandBase[F]{
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("niri::endObserve"))
  }

  object configDCCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("niri::obsSetup"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "niri::observeCmd", s"${NiriTop}dc:apply", s"${NiriTop}dc:applyC", s"${NiriTop}dc:observeC",
    true, s"${NiriTop}dc:stop", s"${NiriTop}dc:abort", ""))

  object stopCmd extends EpicsCommandBase[F]{
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::abort"))

  object abortCmd extends EpicsCommandBase[F]{
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

    private val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  private val status: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::status")

  def beamSplitter: F[String] = safeAttributeF(status.getStringAttribute("BEAMSPLT"))

  def focus: F[String] = safeAttributeF(status.getStringAttribute("FOCUSNAM"))

  def focusPosition: F[Double] = safeAttributeSDoubleF(status.getDoubleAttribute("FOCUSPOS"))

  def mask: F[String] = safeAttributeF(status.getStringAttribute("FPMASK"))

  def pupilViewer: F[String] = safeAttributeF(status.getStringAttribute("PVIEW"))

  def camera: F[String] = safeAttributeF(status.getStringAttribute("CAMERA"))

  def windowCover: F[String] = safeAttributeF(status.getStringAttribute("WINDCOVR"))

  def filter1: F[String] = safeAttributeF(status.getStringAttribute("FILTER1"))

  def filter2: F[String] = safeAttributeF(status.getStringAttribute("FILTER2"))

  def filter3: F[String] = safeAttributeF(status.getStringAttribute("FILTER3"))

  private val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::dcstatus")

  def dhsConnected: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("dhcConnected")).map(_ === 1)

  private val arrayActiveAttr: CaAttribute[JDetectorState] =
    dcStatus.addEnum("arrayState", s"${NiriTop}dc:activate", classOf[JDetectorState])

  def arrayActive: F[Boolean] = safeAttributeF(arrayActiveAttr).map(_.getActive)

  def minIntegration: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("minInt"))

  def integrationTime: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("intTime"))

  def coadds: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("numCoAdds"))

  def detectorTemp: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("TDETABS"))

  def µcodeName: F[String] = safeAttributeF(dcStatus.getStringAttribute("UCODENAM"))

  def µcodeType: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("UCODETYP"))

  def framesPerCycle: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("FRMSPCYCL"))

  def detectorVDetBias: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VDET"))

  def detectorVSetBias: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VSET"))

  def obsEpoch: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("OBSEPOCH"))

  def mountTemp: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("TMOUNT"))

  def digitalAverageCount: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("NDAVGS"))

  def vggCl1: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VGGCL1"))

  def vddCl1: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VDDCL1"))

  def vggCl2: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VGGCL2"))

  def vddCl2: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VDDCL2"))

  def vddUc: F[Double] = safeAttributeSDoubleF(dcStatus.getDoubleAttribute("VDDUC"))

  def lnrs: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("LNRS"))

  def hdrTiming: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("hdrtiming"))

  def arrayType: F[String] = safeAttributeF(dcStatus.getStringAttribute("ARRAYTYP"))

  def arrayId: F[String] = safeAttributeF(dcStatus.getStringAttribute("ARRAYID"))

  def mode: F[Int] = safeAttributeSIntF(dcStatus.getIntegerAttribute("MODE"))

  def dcIsPreparing: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("prepObs")).map(_ =!= 0)

  def dcIsAcquiring: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("acqObs")).map(_ =!= 0)

  def dcIsReadingOut: F[Boolean] = safeAttributeSIntF(dcStatus.getIntegerAttribute("readingOut")).map(_ =!= 0)
}

object NiriEpics extends EpicsSystem[NiriEpics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Niri.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[NiriEpics[IO]] =
    Sync[F].delay(new NiriEpics[IO](service, tops))

}
