// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
import edu.gemini.epics.acm.CaService
import seqexec.server.EpicsHealth
import seqexec.server.EpicsSystem
import seqexec.server.EpicsUtil._
import squants.MetricSystem.Milli
import squants.motion.Bars
import squants.motion.MetersPerSecond
import squants.motion.Pressure
import squants.space.Angle
import squants.space.Degrees
import squants.thermal.Celsius
import squants.Temperature
import squants.Velocity

/**
  * GwsEpics wraps the non-functional parts of the EPICS ACM library to interact with the Weather Server.
  * It has all the objects used to read TCS status values and execute TCS commands.
  */
final class GwsEpics[F[_]: Sync] private (epicsService: CaService) {
  private val state = epicsService.getStatusAcceptor("gws::state")

  private def readD(name: String): F[Double] =
    safeAttributeSDoubleF[F](state.getDoubleAttribute(name))
  private def readI(name: String): F[Int] =
    safeAttributeSIntF[F](state.getIntegerAttribute(name))

  def humidity: F[Double] = readD("humidity").map(_.doubleValue)
  def windVelocity: F[Velocity] =
    readD("windspee").map(v => MetersPerSecond(v.doubleValue))
  def airPressure: F[Pressure] =
    readD("pressure").map(v => Bars(v.doubleValue * Milli))
  def ambientT: F[Temperature] =
    readD("tambient").map(v => Celsius(v.doubleValue))
  def health: F[EpicsHealth] =
    readI("health").map(h => EpicsHealth.fromInt(h.intValue))
  def dewPoint: F[Temperature] =
    readD("dewpoint").map(v => Celsius(v.doubleValue))
  def windDirection: F[Angle] =
    readD("winddire").map(v => Degrees(v.doubleValue))

}

object GwsEpics extends EpicsSystem[GwsEpics[IO]] {
  override val className: String      = getClass.getName
  override val CA_CONFIG_FILE: String = "/Gws.xml"

  override def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[GwsEpics[IO]] =
    Sync[F].delay(new GwsEpics(service))
}
