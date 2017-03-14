package edu.gemini.seqexec.server

import java.util.logging.Logger

import squants.{Temperature, Velocity}
import edu.gemini.epics.acm.{CaService, XMLBuilder}
import squants.motion.{Bars, MetersPerSecond, Pressure}
import squants.MetricSystem.Milli
import squants.space.{Angle, Degrees}
import squants.thermal.Celsius

/**
  * GwsEpics wraps the non-functional parts of the EPICS ACM library to interact with the Weather Server.
  * It has all the objects used to read TCS status values and execute TCS commands.
  *
  * Created by jluhrs on 3/10/17.
  */
final class GwsEpics private (epicsService: CaService) {
  private val state = epicsService.getStatusAcceptor("gws::state")

  def humidity: Option[Double] = Option(state.getDoubleAttribute("humidity").value).map(_.doubleValue)
  def windVelocity: Option[Velocity] = Option(state.getDoubleAttribute("windspee").value).map(v => MetersPerSecond(v.doubleValue))
  def airPressure: Option[Pressure] = Option(state.getDoubleAttribute("pressure").value).map(v => Bars(v.doubleValue*Milli))
  def ambientT: Option[Temperature] = Option(state.getDoubleAttribute("tambient").value).map(v => Celsius(v.doubleValue))
  def health: Option[String] = Option(state.getStringAttribute("health").value)
  def dewPoint: Option[Temperature] = Option(state.getDoubleAttribute("dewpoint").value).map(v => Celsius(v.doubleValue))
  def windDirection: Option[Angle] = Option(state.getDoubleAttribute("winddire").value).map(v => Degrees(v.doubleValue))

}

object GwsEpics extends EpicsSystem {
  val Log = Logger.getLogger(getClass.getName)
  val CA_CONFIG_FILE = "/Gws.xml"
  val GWS_TOP = "ws:"

  private var instanceInternal = Option.empty[GwsEpics]
  lazy val instance: GwsEpics = instanceInternal.getOrElse(
    throw new Exception("Attempt to reference TcsEpics single instance before initialization."))

  override def init(service: CaService): TrySeq[Unit] = {
    try {
      (new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .withCaService(service)
        .withTop("gws", GWS_TOP)
        .buildAll()

        instanceInternal = Some(new GwsEpics(service))

        TrySeq(())

    } catch {
      case c: Throwable =>
        Log.warning("GwsEpics: Problem initializing EPICS service: " + c.getMessage + "\n"
          + c.getStackTrace.mkString("\n"))
        TrySeq.fail(SeqexecFailure.SeqexecException(c))
    }
  }
}
