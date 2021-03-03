// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import cats._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import lucuma.core.enum.KeywordName
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.EpicsHealth
import seqexec.server.keywords._

object GwsHeader {
  def header[F[_]: MonadError[*[_], Throwable]: Logger](
    kwClient:  KeywordsClient[F],
    gwsReader: GwsKeywordReader[F]
  ): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      gwsReader.health
        .map(_ === EpicsHealth.Good)
        .handleError(_ => false) // error check the health read
        .ifM(
          sendKeywords[F](
            id,
            kwClient,
            List(
              buildDouble(gwsReader.humidity, KeywordName.HUMIDITY),
              buildDouble(gwsReader.temperature.map(_.toCelsiusScale), KeywordName.TAMBIENT),
              buildDouble(gwsReader.temperature.map(_.toFahrenheitScale), KeywordName.TAMBIEN2),
              buildDouble(gwsReader.airPressure.map(_.toMillimetersOfMercury),
                          KeywordName.PRESSURE
              ),
              buildDouble(gwsReader.airPressure.map(_.toPascals), KeywordName.PRESSUR2),
              buildDouble(gwsReader.dewPoint.map(_.toCelsiusScale), KeywordName.DEWPOINT),
              buildDouble(gwsReader.dewPoint.map(_.toFahrenheitScale), KeywordName.DEWPOIN2),
              buildDouble(gwsReader.windVelocity.map(_.toMetersPerSecond), KeywordName.WINDSPEE),
              buildDouble(gwsReader.windVelocity.map(_.toInternationalMilesPerHour),
                          KeywordName.WINDSPE2
              ),
              buildDouble(gwsReader.windDirection.map(_.toDegrees), KeywordName.WINDDIRE)
            )
          ),
          Applicative[F].unit
        )

    override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
  }
}
