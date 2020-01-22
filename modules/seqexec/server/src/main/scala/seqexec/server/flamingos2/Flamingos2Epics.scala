// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.effect.{Async, IO, Sync}
import cats.implicits._
import edu.gemini.epics.acm._
import seqexec.model.enum.ApplyCommandResult
import seqexec.server.{EpicsCommandBase, EpicsSystem}
import seqexec.server.EpicsCommandBase.setParameter
import seqexec.server.EpicsUtil._

import scala.concurrent.duration.FiniteDuration

final class Flamingos2Epics[F[_]: Async](epicsService: CaService, tops: Map[String, String]) {

  val F2Top: String = tops.getOrElse("f2", "f2:")

  def post(timeout: FiniteDuration): F[ApplyCommandResult] = configCmd.post(timeout)

  object dcConfigCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::dcconfig"))

    private val biasMode = cs.map(_.getString("biasMode"))
    def setBiasMode(v: String): F[Unit] = setParameter(biasMode, v)

    private val numReads = cs.map(_.getInteger("numReads"))
    def setNumReads(v: Integer): F[Unit] = setParameter(numReads, v)

    private val readoutMode = cs.map(_.getString("readoutMode"))
    def setReadoutMode(v: String): F[Unit] = setParameter(readoutMode, v)

    private val exposureTime: Option[CaParameter[java.lang.Double]] = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): F[Unit] = setParameter[F, java.lang.Double](exposureTime, v)

  }

  object abortCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::abort"))
  }

  object stopCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::stop"))
  }

  object observeCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::observe"))

    private val label = cs.map(_.getString("label"))
    def setLabel(v: String): F[Unit] = setParameter(label, v)
  }

  object endObserveCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::endObserve"))
  }

  object configCmd extends EpicsCommandBase {
    override val cs: Option[CaCommandSender] = Option(epicsService.getCommandSender("flamingos2::config"))

    private val useElectronicOffsetting = cs.map(_.addInteger("useElectronicOffsetting",
      s"${F2Top}wfs:followA.K", "Enable electronic Offsets", false))
    def setUseElectronicOffsetting(v: Integer): F[Unit] = setParameter(useElectronicOffsetting, v)

    private val filter = cs.map(_.getString("filter"))
    def setFilter(v: String): F[Unit] = setParameter(filter, v)

    private val mos = cs.map(_.getString("mos"))
    def setMOS(v: String): F[Unit] = setParameter(mos, v)

    private val grism = cs.map(_.getString("grism"))
    def setGrism(v: String): F[Unit] = setParameter(grism, v)

    private val mask = cs.map(_.getString("mask"))
    def setMask(v: String): F[Unit] = setParameter(mask, v)

    private val decker = cs.map(_.getString("decker"))
    def setDecker(v: String): F[Unit] = setParameter(decker, v)

    private val lyot = cs.map(_.getString("lyot"))
    def setLyot(v: String): F[Unit] = setParameter(lyot, v)

    private val windowCover = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): F[Unit] = setParameter(windowCover, v)

  }

  private val f2State = epicsService.getStatusAcceptor("flamingos2::status")

  private def read(name: String): F[String] =
    safeAttributeF(f2State.getStringAttribute(name))

  private def readI(name: String): F[Int] =
    safeAttributeSIntF[F](f2State.getIntegerAttribute(name))

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
    safeAttributeF(observeCAttr)

  // For FITS keywords
  def health: F[String] =
    read("INHEALTH")

  def state: F[String] =
    read("INSTATE")

  def dcIsPreparing: F[Boolean] = readI(name = "prepObs").map(_ =!= 0)

  def dcIsAcquiring: F[Boolean] = readI(name = "acqObs").map(_ =!= 0)

  def dcIsReadingOut: F[Boolean] = readI(name = "readingOut").map(_ =!= 0)

}

object Flamingos2Epics extends EpicsSystem[Flamingos2Epics[IO]] {

  override val className: String = getClass.getName
  override val CA_CONFIG_FILE: String = "/Flamingos2.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[Flamingos2Epics[IO]] =
    Sync[F].delay(new Flamingos2Epics[IO](service, tops))
}
