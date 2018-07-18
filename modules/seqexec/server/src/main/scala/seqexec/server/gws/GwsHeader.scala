// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import cats.effect.IO
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.{EpicsHealth, SeqAction}

object GwsHeader {
  def headerProvider(dhs: DhsClient): HeaderProvider[GwsHeader.type] = new HeaderProvider[GwsHeader.type] {
    def keywordsClient(a: GwsHeader.type): KeywordsClient[IO] = StandaloneDhsClient(dhs)
  }

  def header[A: HeaderProvider](inst: A, gwsReader: GwsKeywordReader): Header = new Header {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqAction[Unit] = {
      gwsReader.getHealth.flatMap{
        case Some(EpicsHealth.Good) => sendKeywords(id, inst, List(
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
        case _       => SeqAction.void
      }
    }

    override def sendAfter(id: ImageFileId): SeqAction[Unit] = SeqAction(())
  }
}
