package edu.gemini.seqexec.server

import java.util.logging.Logger

import scalaz._
import Scalaz._

import edu.gemini.epics.acm.{XMLBuilder, CaService}

/**
 * Created by jluhrs on 11/13/15.
 */
object Flamingos2Epics {

  import EpicsCommand.setParameter

  def post: SeqAction[Unit] = configCmd.post

  object dcConfigCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("flamingos2::dcconfig"))

    val biasMode = cs.map(_.getString("biasMode"))
    def setBiasMode(v: String): SeqAction[Unit] = setParameter(biasMode, v)

    val numReads = cs.map(_.getInteger("numReads"))
    def setNumReads(v: Integer): SeqAction[Unit] = setParameter(numReads, v)

    val readoutMode = cs.map(_.getString("readoutMode"))
    def setReadoutMode(v: String): SeqAction[Unit] = setParameter(readoutMode, v)

    val exposureTime = cs.map(_.getDouble("exposureTime"))
    def setExposureTime(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](exposureTime, v)

  }

  object abortCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("flamingos2::abort"))
  }

  object stopCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("flamingos2::stop"))
  }

  object observeCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("flamingos2::observe"))

    val label = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)
  }

  object configCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("flamingos2::config"))

    val useElectronicOffsetting = cs.map(_.getInteger("useElectronicOffsetting"))
    def setUseElectronicOffsetting(v: Integer): SeqAction[Unit] = setParameter(useElectronicOffsetting, v)

    val filter = cs.map(_.getString("filter"))
    def setFilter(v: String): SeqAction[Unit] = setParameter(filter, v)

    val mos = cs.map(_.getString("mos"))
    def setMOS(v: String): SeqAction[Unit] = setParameter(mos, v)

    val grism = cs.map(_.getString("grism"))
    def setGrism(v: String): SeqAction[Unit] = setParameter(grism, v)

    val mask = cs.map(_.getString("mask"))
    def setMask(v: String): SeqAction[Unit] = setParameter(mask, v)

    val decker = cs.map(_.getString("decker"))
    def setDecker(v: String): SeqAction[Unit] = setParameter(decker, v)

    val lyot = cs.map(_.getString("lyot"))
    def setLyot(v: String): SeqAction[Unit] = setParameter(lyot, v)

    val windowCover = cs.map(_.getString("windowCover"))
    def setWindowCover(v: String): SeqAction[Unit] = setParameter(windowCover, v)

  }

  val f2State = CaService.getInstance().getStatusAcceptor("flamingos2::dcstatus")

  def exposureTime: Option[String] = Option(f2State.getStringAttribute("exposureTime").value)

  //def useElectronicOffsetting: Option[Integer] = Option(f2State.getIntegerAttribute("useElectronicOffsetting").value)

  def filter: Option[String] = Option(f2State.getStringAttribute("filter").value)

  def mos: Option[String] = Option(f2State.getStringAttribute("mos").value)

  def grism: Option[String] = Option(f2State.getStringAttribute("grism").value)

  def mask: Option[String] = Option(f2State.getStringAttribute("mask").value)

  def decker: Option[String] = Option(f2State.getStringAttribute("decker").value)

  def lyot: Option[String] = Option(f2State.getStringAttribute("lyot").value)

  def windowCover: Option[String] = Option(f2State.getStringAttribute("windowCover").value)

}

object Flamingos2EpicsInitializer {
  val Log = Logger.getLogger(getClass.getName)
  val CA_CONFIG_FILE = "/Flamingos2.xml"

  def init: TrySeq[Unit] = {
    try {
      (new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .buildAll()

      val f2State = CaService.getInstance().getStatusAcceptor("flamingos2::status")

      TrySeq(())
    } catch {
      case c: Throwable =>
        Log.warning("Flamingos2Epics: Problem initializing EPICS service: " + c.getMessage)
        SeqexecFailure.SeqexecException(c).left
    }
  }

  def cleanup(): Unit = {
    CaService.getInstance().unbind()
  }
}