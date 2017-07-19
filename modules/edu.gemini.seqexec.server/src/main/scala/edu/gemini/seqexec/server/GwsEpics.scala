package edu.gemini.seqexec.server

import java.util.logging.Logger

import squants.MetricSystem.Milli
import squants.motion.{Bars, MetersPerSecond, Pressure}
import squants.space.{Angle, Degrees}
import squants.thermal.Celsius
import squants.{Temperature, Velocity}
import edu.gemini.epics.acm.CaService

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
  def health: Option[EpicsHealth] = Option(state.getIntegerAttribute("health").value).map(h => EpicsHealth.fromInt(h.intValue))
  def dewPoint: Option[Temperature] = Option(state.getDoubleAttribute("dewpoint").value).map(v => Celsius(v.doubleValue))
  def windDirection: Option[Angle] = Option(state.getDoubleAttribute("winddire").value).map(v => Degrees(v.doubleValue))

}

object GwsEpics extends EpicsSystem[GwsEpics] {
  override val className: String = getClass.getName
  override val Log = Logger.getLogger(className)
  override val CA_CONFIG_FILE = "/Gws.xml"

  override def build(service: CaService, tops: Map[String, String]) = new GwsEpics(service)
}
