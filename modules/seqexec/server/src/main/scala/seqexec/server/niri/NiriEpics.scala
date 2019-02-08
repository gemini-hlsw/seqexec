// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.IO
import cats.effect.Sync
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
import seqexec.server.EpicsCommand.setParameterF
import seqexec.server.{EpicsCommandF, EpicsSystem, ObserveCommand, ObserveCommandF}
import org.log4s.{Logger, getLogger}

class NiriEpics[F[_]: Sync](epicsService: CaService, tops: Map[String, String]) {
  private val F: Sync[F] = Sync[F]

  val NiriTop = tops.getOrElse("niri", "niri:")
  val NisTop = tops.getOrElse("nis", "NIS:")

  object configCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("nis::config"))

    private val disperser: Option[CaParameter[JDisperser]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("disperser", s"${NisTop}grism:menu", classOf[JDisperser], false)))
    def setDisperser(v: JDisperser): F[Unit] = setParameterF(disperser, v)

    private val readMode: Option[CaParameter[JReadMode]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("readmode", s"${NisTop}readmode:menu", classOf[JReadMode], false)))
    def setReadMode(v: JReadMode): F[Unit] = setParameterF(readMode, v)

    private val coadds: Option[CaParameter[Integer]] = cs.flatMap(cmd => Option(cmd.getInteger("numCoAdds")))
    def setCoadds(v: Int): F[Unit] = setParameterF(coadds, Integer.valueOf(v))

    private val mask: Option[CaParameter[JMask]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("mask", s"${NisTop}fpmask:menu", classOf[JMask], false)))
    def setMask(v: JMask): F[Unit] = setParameterF(mask, v)

    private val camera: Option[CaParameter[JCamera]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("camera", s"${NisTop}camera:menu", classOf[JCamera], false)))
    def setCamera(v: JCamera): F[Unit] = setParameterF(camera, v)

    private val beamSplitter: Option[CaParameter[JBeamSplitter]] = cs.flatMap(cmd => Option(
      cmd.addEnum("beamSplitter", s"${NisTop}beamsplit:menu", classOf[JBeamSplitter], false)))
    def setBeamSplitter(v: JBeamSplitter): F[Unit] = setParameterF(beamSplitter, v)

    private val exposureTime: Option[CaParameter[JDouble]] = cs.flatMap(cmd =>
      Option(cmd.getDouble("exposureTime")))
    def setExposureTime(v: Double): F[Unit] = setParameterF(exposureTime, JDouble.valueOf(v))

    private val builtInROI: Option[CaParameter[JBuiltInROI]] = cs.flatMap(cmd =>
      Option(cmd.addEnum("builtinROI", s"${NisTop}roi:menu", classOf[JBuiltInROI], false)))
    def setBuiltInROI(v: JBuiltInROI): F[Unit] = setParameterF(builtInROI, v)

    private val filter: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString("filter")))
    def setFilter(v: String): F[Unit] = setParameterF(filter, v)

    private val focus: Option[CaParameter[String]] = cs.flatMap(cmd => Option(cmd.getString
    ("focus")))
    def setFocus(v: String): F[Unit] = setParameterF(focus, v)

  }

  /*
   * For some reason the window cover is not include in the IS configuration parameters. It is
   * applied by the IS apply command, nevertheless. This command exists only to set the parameter.
   */
  object windowCoverConfig extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri:config"))

    private val windowCover: Option[CaParameter[String]] = cs.flatMap(cmd =>
      Option(cmd.getString("windowCover")))
    def setWindowCover(v: String): F[Unit] = setParameterF(windowCover, v)
  }

  object endObserveCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("niri::endObserve"))
  }

  object configDCCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] =
      Option(epicsService.getCommandSender("niri::obsSetup"))
  }

  private val stopCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::stop"))
  private val observeAS: Option[CaApplySender] = Option(epicsService.createObserveSender(
    "niri::observeCmd", s"${NiriTop}dc:apply", s"${NiriTop}dc:applyC", s"${NiriTop}dc:observeC",
    true, s"${NiriTop}dc:stop", s"${NiriTop}dc:abort", ""))

  object stopCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = stopCS
  }

  object stopAndWaitCmd extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = stopCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  private val abortCS: Option[CaCommandSender] = Option(epicsService.getCommandSender("niri::abort"))

  object abortCmd extends EpicsCommandF {
    override protected val cs: Option[CaCommandSender] = abortCS
  }

  object abortAndWait extends ObserveCommand {
    override protected val cs: Option[CaCommandSender] = abortCS
    override protected val os: Option[CaApplySender] = observeAS
  }

  object observeCmd extends ObserveCommandF {
    override protected val cs: Option[CaCommandSender] = Option(
      epicsService.getCommandSender("niri::observe"))
    override protected val os: Option[CaApplySender] = observeAS

    private val label: Option[CaParameter[String]] = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameterF(label, v)
  }

  private val status: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::status")

  def beamSplitter: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("BEAMSPLT"))
        .flatMap(x => Option(x.value))
    )

  def focus: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("FOCUSNAM"))
        .flatMap(x => Option(x.value))
    )

  def focusPosition: F[Option[Double]] =
    F.delay(
      Option(status.getDoubleAttribute("FOCUSPOS"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def mask: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("FPMASK"))
        .flatMap(x => Option(x.value))
    )

  def pupilViewer: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("PVIEW"))
        .flatMap(x => Option(x.value))
    )

  def camera: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("CAMERA"))
        .flatMap(x => Option(x.value))
    )

  def windowCover: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("WINDCOVR"))
        .flatMap(x => Option(x.value))
    )

  def filter1: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("FILTER1"))
        .flatMap(x => Option(x.value))
    )

  def filter2: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("FILTER2"))
        .flatMap(x => Option(x.value))
    )

  def filter3: F[Option[String]] =
    F.delay(
      Option(status.getStringAttribute("FILTER3"))
        .flatMap(x => Option(x.value))
    )

  private val dcStatus: CaStatusAcceptor = epicsService.getStatusAcceptor("niri::dcstatus")

  def dhsConnected: F[Option[Boolean]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("dhcConnected"))
        .flatMap(x => Option(x.value).map(_.toInt === 1))
    )

  private val arrayActiveAttr: Option[CaAttribute[JDetectorState]] =
    Option(dcStatus.addEnum("arrayState", s"${NiriTop}dc:activate", classOf[JDetectorState]))

  def arrayActive: F[Option[Boolean]] =
    F.delay(
      arrayActiveAttr.flatMap(at => Option(at.value).map(_.getActive))
    )

  def minIntegration: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("minInt"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def integrationTime: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("intTime"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def coadds: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("numCoAdds"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def detectorTemp: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("TDETABS"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def µcodeName: F[Option[String]] =
    F.delay(
      Option(dcStatus.getStringAttribute("UCODENAM"))
        .flatMap(x => Option(x.value))
    )

  def µcodeType: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("UCODETYP"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def framesPerCycle: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("FRMSPCYCL"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def detectorVDetBias: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VDET"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def detectorVSetBias: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VSET"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def obsEpoch: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("OBSEPOCH"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def mountTemp: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("TMOUNT"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def digitalAverageCount: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("NDAVGS"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def vggCl1: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VGGCL1"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def vddCl1: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VDDCL1"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def vggCl2: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VGGCL2"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def vddCl2: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VDDCL2"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def vddUc: F[Option[Double]] =
    F.delay(
      Option(dcStatus.getDoubleAttribute("VDDUC"))
        .flatMap(x => Option(x.value).map(_.toDouble))
    )

  def lnrs: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("LNRS"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def hdrTiming: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("hdrtiming"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

  def arrayType: F[Option[String]] =
    F.delay(
      Option(dcStatus.getStringAttribute("ARRAYTYP"))
        .flatMap(x => Option(x.value))
    )

  def arrayId: F[Option[String]] =
    F.delay(
      Option(dcStatus.getStringAttribute("ARRAYID"))
        .flatMap(x => Option(x.value))
    )

  def mode: F[Option[Int]] =
    F.delay(
      Option(dcStatus.getIntegerAttribute("MODE"))
        .flatMap(x => Option(x.value).map(_.toInt))
    )

}

object NiriEpics extends EpicsSystem[NiriEpics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Niri.xml"

  override def build(service: CaService, tops: Map[String, String]) =
    new NiriEpics[IO](service, tops)

}
