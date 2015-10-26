package edu.gemini.seqexec.server

import java.util
import java.util.logging.Logger
import edu.gemini.seqexec.server.TcsEpics._
import edu.gemini.seqexec.server.tcs.{BinaryYesNo, BinaryOnOff}

import collection.JavaConversions._

import edu.gemini.epics.acm._

import scala.concurrent.duration.Duration
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS. It has all the objects used
 * to read TCS status values and execute TCS commands.
 *
 * Created by jluhrs on 10/1/15.
 */

final class TcsEpics private () {
  import TcsEpics._
  import EpicsCommand.setParameter


  // This is a bit ugly. Commands are triggered from the main apply record, so I just choose an arbitrary command here.
  // Triggering that command will trigger all the marked commands.
  def post: SeqAction[Unit] = m1GuideCmd.post

  object m1GuideCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("m1Guide"))
    val state = cs.map(_.getString("state"))
    def setState(v: String): SeqAction[Unit] = setParameter(state, v)
  }

  object m2GuideCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("m2Guide"))
    val state = cs.map(_.getString("state"))
    def setState(v: String): SeqAction[Unit] = setParameter[String](state, v)
  }

  object mountGuideCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("mountGuide"))

    val source = cs.map(_.getString("source"))
    def setSource(v: String): SeqAction[Unit] = setParameter(source, v)

    val p1weight = cs.map(_.getDouble("p1weight"))
    def setP1Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p1weight, v)

    val p2weight = cs.map(_.getDouble("p2weight"))
    def setP2Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p2weight, v)

    val mode = cs.map(_.getString("mode"))
    def setMode(v: String): SeqAction[Unit] = setParameter(mode, v)
  }

  object offsetACmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoA1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetBCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoB1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetCCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoC1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object wavelSourceA extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("wavelSourceA"))

    val wavel = cs.map(_.getDouble("wavel"))
    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceB extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("wavelSourceB"))

    val wavel = cs.map(_.getDouble("wavel"))
    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object wavelSourceC extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("wavelSourceC"))

    val wavel = cs.map(_.getDouble("wavel"))
    def setWavel(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](wavel, v)
  }

  object m2Beam extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("m2Beam"))

    val beam = cs.map(_.getString("beam"))
    def setBeam(v: String): SeqAction[Unit] = setParameter(beam, v)
  }

  object pwfs1ProbeGuideCmd extends ProbeGuideCmd("pwfs1Guide")
  object pwfs2ProbeGuideCmd extends ProbeGuideCmd("pwfs2Guide")
  object oiwfsProbeGuideCmd extends ProbeGuideCmd("oiwfsGuide")

  object pwfs1Park extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("pwfs1Park"))
  }

  object pwfs2Park extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("pwfs2Park"))
  }

  object oiwfsPark extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("oiwfsPark"))
  }

  object pwfs1StopObserveCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("pwfs1StopObserve"))
  }

  object pwfs2StopObserveCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("pwfs2StopObserve"))
  }

  object oiwfsStopObserveCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("oiwfsStopObserve"))
  }

  object pwfs1ObserveCmd extends WfsObserveCmd("pwfs1Observe")
  object pwfs2ObserveCmd extends WfsObserveCmd("pwfs2Observe")
  object oiwfsObserveCmd extends WfsObserveCmd("oiwfsObserve")

  object hrwfsParkCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("hrwfsPark"))
  }

  object hrwfsPosCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("hrwfs"))

    val hrwfsPos = cs.map(_.getString("hrwfsPos"))
    def setHrwfsPos(v: String): SeqAction[Unit] = setParameter(hrwfsPos, v)
  }

  object scienceFoldParkCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("scienceFoldPark"))
  }

  object scienceFoldPosCmd extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("scienceFold"))

    val scfold = cs.map(_.getString("scfold"))
    def setScfold(v: String): SeqAction[Unit] = setParameter(scfold, v)
  }

  val tcsState = CaService.getInstance().getStatusAcceptor("tcsstate")

  def absorbTipTilt: Option[Integer] = Option(tcsState.getIntegerAttribute("absorbTipTilt").value)
  def m1GuideSource: Option[String] = Option(tcsState.getStringAttribute("m1GuideSource").value)
  private val m1GuideAttr: Option[CaAttribute[BinaryOnOff]] = Option(tcsState.addEnum("m1Guide",
    TCS_TOP + "im:m1GuideOn", classOf[BinaryOnOff], "M1 guide"))
  def m1Guide: Option[BinaryOnOff] = m1GuideAttr.flatMap(v => Option(v.value))
  def m2p1Guide: Option[String] = Option(tcsState.getStringAttribute("m2p1Guide").value)
  def m2p2Guide: Option[String] = Option(tcsState.getStringAttribute("m2p2Guide").value)
  def m2oiGuide: Option[String] = Option(tcsState.getStringAttribute("m2oiGuide").value)
  def m2aoGuide: Option[String] = Option(tcsState.getStringAttribute("m2aoGuide").value)
  def comaCorrect: Option[String] = Option(tcsState.getStringAttribute("comaCorrect").value)
  private val m2GuideStateAttr: Option[CaAttribute[BinaryOnOff]] = Option(tcsState.addEnum("m2GuideState",
    TCS_TOP + "om:m2GuideState", classOf[BinaryOnOff], "M2 guiding state"))
  def m2GuideState: Option[BinaryOnOff] = m2GuideStateAttr.flatMap(v => Option(v.value))
  def xoffsetPoA1: Option[Double] = Option(tcsState.getDoubleAttribute("xoffsetPoA1").value).map(_.doubleValue)
  def yoffsetPoA1: Option[Double] = Option(tcsState.getDoubleAttribute("yoffsetPoA1").value).map(_.doubleValue)
  def xoffsetPoB1: Option[Double] = Option(tcsState.getDoubleAttribute("xoffsetPoB1").value).map(_.doubleValue)
  def yoffsetPoB1: Option[Double] = Option(tcsState.getDoubleAttribute("yoffsetPoB1").value).map(_.doubleValue)
  def xoffsetPoC1: Option[Double] = Option(tcsState.getDoubleAttribute("xoffsetPoC1").value).map(_.doubleValue)
  def yoffsetPoC1: Option[Double] = Option(tcsState.getDoubleAttribute("yoffsetPoC1").value).map(_.doubleValue)
  def sourceAWavelength: Option[Double] = Option(tcsState.getDoubleAttribute("sourceAWavelength").value).map(_.doubleValue)
  def sourceBWavelength: Option[Double] = Option(tcsState.getDoubleAttribute("sourceBWavelength").value).map(_.doubleValue)
  def sourceCWavelength: Option[Double] = Option(tcsState.getDoubleAttribute("sourceCWavelength").value).map(_.doubleValue)
  def chopBeam: Option[String] = Option(tcsState.getStringAttribute("chopBeam").value)
  def p1FollowS: Option[String] = Option(tcsState.getStringAttribute("p1FollowS").value)
  def p2FollowS: Option[String] = Option(tcsState.getStringAttribute("p2FollowS").value)
  def oiFollowS: Option[String] = Option(tcsState.getStringAttribute("oiFollowS").value)
  private val pwfs1OnAttr: Option[CaAttribute[BinaryYesNo]] = Option(tcsState.addEnum("pwfs1On",
    TCS_TOP + "drives:p1Integrating", classOf[BinaryYesNo], "P1 integrating"))
  def pwfs1On: Option[BinaryYesNo] = pwfs1OnAttr.flatMap(v => Option(v.value))
  private val pwfs2OnAttr: Option[CaAttribute[BinaryYesNo]] = Option(tcsState.addEnum("pwfs2On",
    TCS_TOP + "drives:p2Integrating", classOf[BinaryYesNo], "P2 integrating"))
  def pwfs2On: Option[BinaryYesNo] = pwfs2OnAttr.flatMap(v => Option(v.value))
  private val oiwfsOnAttr: Option[CaAttribute[BinaryYesNo]] = Option(tcsState.addEnum("oiwfsOn",
    TCS_TOP + "drives:oiIntegrating", classOf[BinaryYesNo], "P2 integrating"))
  def oiwfsOn: Option[BinaryYesNo] = oiwfsOnAttr.flatMap(v => Option(v.value))
  def aowfsOn: Option[Double] = Option(tcsState.getDoubleAttribute("aowfsOn").value).map(_.doubleValue)
  def sfName: Option[String] = Option(tcsState.getStringAttribute("sfName").value)
  //def sfParked: Option[Integer] = Option(tcsState.getIntegerAttribute("sfParked").value)
  def sfParked: Option[Integer] = Some(0)
  def agHwName: Option[String] = Option(tcsState.getStringAttribute("agHwName").value)
//  def agHwParked: Option[Integer] = Option(tcsState.getIntegerAttribute("agHwParked").value)
  def agHwParked: Option[Integer] = Some(1)
  def instrAA: Option[Double] = Option(tcsState.getDoubleAttribute("instrAA").value).map(_.doubleValue)
  private val inPositionAttr: CaAttribute[String] = tcsState.getStringAttribute("inPosition")
  def inPosition: Option[String] = Option(inPositionAttr.value)

  object pwfs1ProbeGuideConfig extends ProbeGuideConfig("p1", tcsState)
  object pwfs2ProbeGuideConfig extends ProbeGuideConfig("p2", tcsState)
  object oiwfsProbeGuideConfig extends ProbeGuideConfig("oi", tcsState)

  def waitInPosition: SeqAction[Unit] =
    EpicsCommand.safe(Task.async((f) => {
      val statusListener = new CaAttributeListener[String] {

        override def onValueChange(newVals: util.List[String]): Unit = {
          TcsEpicsInitializer.Log.info("inPosition listener called")
          if (!newVals.isEmpty && newVals.get(0) == "TRUE") {
            TcsEpics().inPositionAttr.removeListener(this)
            f(TrySeq(()).right)
          }
        }


        override def onValidityChange(newValidity: Boolean): Unit = {}
      }

      //val timer = Timer(t.toMillis)
      TcsEpics().inPositionAttr.addListener(statusListener)

    } ) )

}

object TcsEpics {
  import EpicsCommand.setParameter

  val TCS_TOP = "tc1:"

  lazy val instance = new TcsEpics

  def apply(): TcsEpics = instance

  sealed class ProbeGuideCmd(csName: String) extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender(csName))

    val nodachopa = cs.map(_.getString("nodachopa"))
    def setNodachopa(v: String): SeqAction[Unit] = setParameter(nodachopa, v)

    val nodachopb = cs.map(_.getString("nodachopb"))
    def setNodachopb(v: String): SeqAction[Unit] = setParameter(nodachopb, v)

    val nodachopc = cs.map(_.getString("nodachopc"))
    def setNodachopc(v: String): SeqAction[Unit] = setParameter(nodachopc, v)

    val nodbchopa = cs.map(_.getString("nodbchopa"))
    def setNodbchopa(v: String): SeqAction[Unit] = setParameter(nodbchopa, v)

    val nodbchopb = cs.map(_.getString("nodbchopb"))
    def setNodbchopb(v: String): SeqAction[Unit] = setParameter(nodbchopb, v)

    val nodbchopc = cs.map(_.getString("nodbchopc"))
    def setNodbchopc(v: String): SeqAction[Unit] = setParameter(nodbchopc, v)

    val nodcchopa = cs.map(_.getString("nodcchopa"))
    def setNodcchopa(v: String): SeqAction[Unit] = setParameter(nodcchopa, v)

    val nodcchopb = cs.map(_.getString("nodcchopb"))
    def setNodcchopb(v: String): SeqAction[Unit] = setParameter(nodcchopb, v)

    val nodcchopc = cs.map(_.getString("nodcchopc"))
    def setNodcchopc(v: String): SeqAction[Unit] = setParameter(nodcchopc, v)
  }

  sealed class WfsObserveCmd(csName: String) extends EpicsCommand {
    override val cs= Option(CaService.getInstance().getCommandSender(csName))

    val noexp = cs.map(_.getInteger("noexp"))
    def setNoexp(v: Integer): SeqAction[Unit] = setParameter(noexp, v)

    val int = cs.map(_.getDouble("int"))
    def setInt(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](int, v)

    val outopt = cs.map(_.getString("outopt"))
    def setOutopt(v: String): SeqAction[Unit] = setParameter(outopt, v)

    val label = cs.map(_.getString("label"))
    def setLabel(v: String): SeqAction[Unit] = setParameter(label, v)

    val output = cs.map(_.getString("output"))
    def setOutput(v: String): SeqAction[Unit] = setParameter(output, v)

    val path = cs.map(_.getString("path"))
    def setPath(v: String): SeqAction[Unit] = setParameter(path, v)

    val name = cs.map(_.getString("name"))
    def setName(v: String): SeqAction[Unit] = setParameter(name, v)
  }

  class ProbeGuideConfig(protected val prefix: String, protected val tcsState: CaStatusAcceptor) {
    def nodachopa: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodachopa").value)
    def nodachopb: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodachopb").value)
    def nodachopc: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodachopc").value)
    def nodbchopa: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodbchopa").value)
    def nodbchopb: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodbchopb").value)
    def nodbchopc: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodbchopc").value)
    def nodcchopa: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodcchopa").value)
    def nodcchopb: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodcchopb").value)
    def nodcchopc: Option[String] = Option(tcsState.getStringAttribute(prefix+"nodcchopc").value)
  }

}

object TcsEpicsInitializer {
  val Log = Logger.getLogger(getClass.getName)
  val CA_CONFIG_FILE = "/Tcs.xml"

  def init: TrySeq[Unit] = {
    try {
      (new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .withTop("tcs", TcsEpics.TCS_TOP)
        .withTop("ag", "tag:")
        .withTop("m2", "tc1:m2:")
        .withTop("ao", "tc1:ao:")
        .withTop("oiwfs", "toiwfs:")
        .buildAll()

      val tcsState = CaService.getInstance().getStatusAcceptor("ẗcsstate")

      TrySeq(())
    } catch {
      case c: Throwable => {
        Log.warning("TcsEpics: Problem initializing EPICS service: " + c.getMessage)
        SeqexecFailure.SeqexecException(c).left
      }
    }
  }

  def cleanup(): Unit = {
    CaService.getInstance().unbind()
  }
}