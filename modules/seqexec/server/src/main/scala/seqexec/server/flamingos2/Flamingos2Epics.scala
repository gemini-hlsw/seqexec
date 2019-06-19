// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.effect.IO
import cats.effect.Async
import edu.gemini.epics.acm._
import seqexec.server.{EpicsCommand, EpicsCommandF, EpicsSystem}
import seqexec.server.EpicsCommand.setParameterF
import seqexec.server.EpicsUtil._
import org.log4s.{Logger, getLogger}

final class Flamingos2Epics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  val F2Top: String = tops.getOrElse("f2", "f2:")

  def post: F[EpicsCommand.Result] = configCmd.post[F]

  object dcConfigCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::dcconfig"))

    private val biasMode = cs.map(_.getString("biasMode"))
    def setBiasMode(v: String): F[Unit] = setParameterF(biasMode, v)

    private val numReads = cs.map(_.getInteger("numReads"))
    def setNumReads(v: Integer): F[Unit] = setParameterF(numReads, v)

    private val readoutMode = cs.map(_.getString("readoutMode"))
    def setReadoutMode(v: String): F[Unit] = setParameterF(readoutMode, v)

    private val exposureTime: Option[CaParameter[java.lang.Double]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameterF[F, java.lang.Double](exposureTime, v)

  }

  object abortCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::abort"))
  }

  object stopCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::stop"))
  }

  object observeCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::observe"))

    private val label = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameterF(label, v)
  }

  object endObserveCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::endObserve"))
  }

  object configCmd extends EpicsCommandF {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::config"))

    private val useElectronicOffsetting = cs.map(_.addInteger("useElectronicOffsetting",
      s"${F2Top}wfs:followA.K", "Enable electronic Offsets", false))
    def setUseElectronicOffsetting(v: Integer): F[Unit] = setParameterF(useElectronicOffsetting, v)

    private val filter = cs.map(_.getString("filter"))
    def setFilter(v: String): F[Unit] = setParameterF(filter, v)

    private val mos = cs.map(_.getString("mos"))
    def setMOS(v: String): F[Unit] = setParameterF(mos, v)

    private val grism = cs.map(_.getString("grism"))
    def setGrism(v: String): F[Unit] = setParameterF(grism, v)

    private val mask = cs.map(_.getString("mask"))
    def setMask(v: String): F[Unit] = setParameterF(mask, v)

    private val decker = cs.map(_.getString("decker"))
    def setDecker(v: String): F[Unit] = setParameterF(decker, v)

    private val lyot = cs.map(_.getString("lyot"))
    def setLyot(v: String): F[Unit] = setParameterF(lyot, v)

    private val windowCover = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): F[Unit] = setParameterF(windowCover, v)

  }

  private val f2State = epicsService.getStatusAcceptor("flamingos2::status")

  private def read(name: String): F[String] =
    safeAttributeF(name, f2State.getStringAttribute(name))

  private def readI(name: String): F[Int] =
    safeAttributeSIntF[F](name, f2State.getIntegerAttribute(name))

  def exposureTime: F[String] =
    read("exposureTime")

  def filter: F[String] =
    read("filter")

  def mos: F[String] =
    read("mos")

  def grism: F[String] =
    read("grism")

  def mask: F[String] =
    read("mask")

  def decker: F[String] =
    read("decker")

  def lyot: F[String] =
    read("lyot")

  def windowCover: F[String] =
    read("windowCover")

  def countdown: F[Int] =
    readI("countdown")

  private val observeCAttr: CaAttribute[CarState] =
    f2State.addEnum("observeState", s"${F2Top}observeC.VAL", classOf[CarState])

  def observeState: F[CarState] =
    safeAttributeF("observeState", observeCAttr)

  // For FITS keywords
  def health: F[String] =
    read("INHEALTH")

  def state: F[String] =
    read("INSTATE")

}

object Flamingos2Epics extends EpicsSystem[Flamingos2Epics[IO]] {

  override val className: String = getClass.getName
  override val Log: Logger = getLogger
  override val CA_CONFIG_FILE: String = "/Flamingos2.xml"

  override def build(service: CaService, tops: Map[String, String]) =
    new Flamingos2Epics[IO](service, tops)
}
