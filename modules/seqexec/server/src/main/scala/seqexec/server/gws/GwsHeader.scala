// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import cats._
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.{EpicsHealth, InstrumentSystem}

object GwsHeader {
  def header[F[_]: MonadError[?[_], Throwable]](inst: InstrumentSystem[F], gwsReader: GwsKeywordReader[F]): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      gwsReader.health.map(_ === EpicsHealth.Good)
        .handleError(_ => false) // error check the health read
        .ifM(sendKeywords[F](id, inst, List(
          buildDouble(gwsReader.humidity, KeywordName.HUMIDITY),
          buildDouble(gwsReader.temperature.map(_.toCelsiusScale), KeywordName.TAMBIENT),
          buildDouble(gwsReader.temperature.map(_.toFahrenheitScale), KeywordName.TAMBIEN2),
          buildDouble(gwsReader.airPressure.map(_.toMillimetersOfMercury), KeywordName.PRESSURE),
          buildDouble(gwsReader.airPressure.map(_.toPascals), KeywordName.PRESSUR2),
          buildDouble(gwsReader.dewPoint.map(_.toCelsiusScale), KeywordName.DEWPOINT),
          buildDouble(gwsReader.dewPoint.map(_.toFahrenheitScale), KeywordName.DEWPOIN2),
          buildDouble(gwsReader.windVelocity.map(_.toMetersPerSecond), KeywordName.WINDSPEE),
          buildDouble(gwsReader.windVelocity.map(_.toInternationalMilesPerHour), KeywordName.WINDSPE2),
          buildDouble(gwsReader.windDirection.map(_.toDegrees), KeywordName.WINDDIRE)
        )), Applicative[F].unit)

    override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
  }
}
