// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.{EpicsHealth, InstrumentSystem}

object GwsHeader {
  def header[F[_]: Sync: cats.Monad](inst: InstrumentSystem[F], gwsReader: GwsKeywordReader[F]): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
      gwsReader.getHealth.liftF.flatMap {
        case Some(EpicsHealth.Good) => sendKeywords[F](id, inst, List(
          buildDouble(gwsReader.getHumidity.orDefault, KeywordName.HUMIDITY),
          {
            val x = gwsReader.getTemperature.map(_.map(_.toCelsiusScale))
            buildDouble(x.orDefault, KeywordName.TAMBIENT)
          },
          {
            val x = gwsReader.getTemperature.map(_.map(_.toFahrenheitScale))
            buildDouble(x.orDefault, KeywordName.TAMBIEN2)
          },
          {
            val x = gwsReader.getAirPressure.map(_.map(_.toMillimetersOfMercury))
            buildDouble(x.orDefault, KeywordName.PRESSURE)
          },
          {
            val x = gwsReader.getAirPressure.map(_.map(_.toPascals))
            buildDouble(x.orDefault, KeywordName.PRESSUR2)
          },
          {
            val x = gwsReader.getDewPoint.map(_.map(_.toCelsiusScale))
            buildDouble(x.orDefault, KeywordName.DEWPOINT)
          },
          {
            val x = gwsReader.getDewPoint.map(_.map(_.toFahrenheitScale))
            buildDouble(x.orDefault, KeywordName.DEWPOIN2)
          },
          {
            val x = gwsReader.getWindVelocity.map(_.map(_.toMetersPerSecond))
            buildDouble(x.orDefault, KeywordName.WINDSPEE)
          },
          {
            val x = gwsReader.getWindVelocity.map(_.map(_.toInternationalMilesPerHour))
            buildDouble(x.orDefault, KeywordName.WINDSPE2)
          },
          {
            val x = gwsReader.getWindDirection.map(_.map(_.toDegrees))
            buildDouble(x.orDefault, KeywordName.WINDDIRE)
          }
        ))
        case _       => Applicative[F].unit
      }
    }

    override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
  }
}
