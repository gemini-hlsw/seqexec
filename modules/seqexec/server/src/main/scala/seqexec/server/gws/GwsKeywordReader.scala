// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import seqexec.server.{EpicsHealth, SeqAction}
import seqexec.server.KeywordsReader._
import squants.motion.{MetersPerSecond, Pressure, StandardAtmospheres}
import squants.space.Degrees
import squants.thermal.Celsius
import squants.{Angle, Temperature, Velocity}

/**
  * Created by jluhrs on 3/13/17.
  */
trait GwsKeywordReader {
  def getHealth: SeqAction[Option[EpicsHealth]]
  def getTemperature: SeqAction[Option[Temperature]]
  def getDewPoint: SeqAction[Option[Temperature]]
  def getAirPressure: SeqAction[Option[Pressure]]
  def getWindVelocity: SeqAction[Option[Velocity]]
  def getWindDirection: SeqAction[Option[Angle]]
  def getHumidity: SeqAction[Option[Double]]
}

object DummyGwsKeywordsReader extends GwsKeywordReader {

  override def getTemperature: SeqAction[Option[Temperature]] = Celsius(15.0)

  override def getDewPoint: SeqAction[Option[Temperature]] = Celsius(1.0)

  override def getAirPressure: SeqAction[Option[Pressure]] = StandardAtmospheres(1.0)

  override def getWindVelocity: SeqAction[Option[Velocity]] = MetersPerSecond(5)

  override def getWindDirection: SeqAction[Option[Angle]] = Degrees(60.0)

  override def getHumidity: SeqAction[Option[Double]] = 20.0

  override def getHealth: SeqAction[Option[EpicsHealth]] = EpicsHealth.Good
}

object GwsKeywordsReaderImpl extends GwsKeywordReader {

  override def getTemperature: SeqAction[Option[Temperature]] = GwsEpics.instance.ambientT

  override def getDewPoint: SeqAction[Option[Temperature]] = GwsEpics.instance.dewPoint

  override def getAirPressure: SeqAction[Option[Pressure]] = GwsEpics.instance.airPressure

  override def getWindVelocity: SeqAction[Option[Velocity]] = GwsEpics.instance.windVelocity

  override def getWindDirection: SeqAction[Option[Angle]] = GwsEpics.instance.windDirection

  override def getHumidity: SeqAction[Option[Double]] = GwsEpics.instance.humidity

  override def getHealth: SeqAction[Option[EpicsHealth]] = GwsEpics.instance.health
}
