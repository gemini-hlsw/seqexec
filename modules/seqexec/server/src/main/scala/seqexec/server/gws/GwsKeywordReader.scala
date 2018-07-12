// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import seqexec.server.{EpicsHealth, SeqAction}
import seqexec.server.keywords._
import squants.motion.{MetersPerSecond, Pressure, StandardAtmospheres}
import squants.space.Degrees
import squants.thermal.Celsius
import squants.{Angle, Temperature, Velocity}

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

  override def getTemperature: SeqAction[Option[Temperature]] = Celsius(15.0).toSeqAction

  override def getDewPoint: SeqAction[Option[Temperature]] = Celsius(1.0).toSeqAction

  override def getAirPressure: SeqAction[Option[Pressure]] = StandardAtmospheres(1.0).toSeqAction

  override def getWindVelocity: SeqAction[Option[Velocity]] = MetersPerSecond(5).toSeqAction

  override def getWindDirection: SeqAction[Option[Angle]] = Degrees(60.0).toSeqAction

  override def getHumidity: SeqAction[Option[Double]] = 20.0.toSeqAction

  override def getHealth: SeqAction[Option[EpicsHealth]] = (EpicsHealth.Good: EpicsHealth).toSeqAction
}

object GwsKeywordsReaderImpl extends GwsKeywordReader {

  override def getTemperature: SeqAction[Option[Temperature]] = GwsEpics.instance.ambientT.toSeqActionO

  override def getDewPoint: SeqAction[Option[Temperature]] = GwsEpics.instance.dewPoint.toSeqActionO

  override def getAirPressure: SeqAction[Option[Pressure]] = GwsEpics.instance.airPressure.toSeqActionO

  override def getWindVelocity: SeqAction[Option[Velocity]] = GwsEpics.instance.windVelocity.toSeqActionO

  override def getWindDirection: SeqAction[Option[Angle]] = GwsEpics.instance.windDirection.toSeqActionO

  override def getHumidity: SeqAction[Option[Double]] = GwsEpics.instance.humidity.toSeqActionO

  override def getHealth: SeqAction[Option[EpicsHealth]] = GwsEpics.instance.health.toSeqActionO
}
