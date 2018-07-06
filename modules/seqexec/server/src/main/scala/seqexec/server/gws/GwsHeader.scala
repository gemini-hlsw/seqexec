// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gws

import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.DhsClient
import seqexec.server.keywords.StandaloneDhsClient
import seqexec.server.keywords.KeywordsClient
import seqexec.server.Header.Implicits._
import seqexec.server.Header._
import seqexec.server.{EpicsHealth, Header, HeaderProvider, SeqAction}

object GwsHeader {
  def headerProvider(dhs: DhsClient): HeaderProvider[GwsHeader.type] = new HeaderProvider[GwsHeader.type] {
    def name(a: GwsHeader.type): String = "gws"
    def keywordsClient(a: GwsHeader.type): KeywordsClient = StandaloneDhsClient(dhs)
  }
  def header[A: HeaderProvider](inst: A, gwsReader: GwsKeywordReader): Header = new Header {
    override def sendBefore(id: ImageFileId): SeqAction[Unit] = {
      gwsReader.getHealth.flatMap{
        case Some(EpicsHealth.Good) => sendKeywords(id, inst, List(
          buildDouble(gwsReader.getHumidity.orDefault, "HUMIDITY"),
          {
            val x = gwsReader.getTemperature.map(_.map(_.toCelsiusScale))
            buildDouble(x.orDefault, "TAMBIENT")
          },
          {
            val x = gwsReader.getTemperature.map(_.map(_.toFahrenheitScale))
            buildDouble(x.orDefault, "TAMBIEN2")
          },
          {
            val x = gwsReader.getAirPressure.map(_.map(_.toMillimetersOfMercury))
            buildDouble(x.orDefault, "PRESSURE")
          },
          {
            val x = gwsReader.getAirPressure.map(_.map(_.toPascals))
            buildDouble(x.orDefault, "PRESSUR2")
          },
          {
            val x = gwsReader.getDewPoint.map(_.map(_.toCelsiusScale))
            buildDouble(x.orDefault, "DEWPOINT")
          },
          {
            val x = gwsReader.getDewPoint.map(_.map(_.toFahrenheitScale))
            buildDouble(x.orDefault, "DEWPOIN2")
          },
          {
            val x = gwsReader.getWindVelocity.map(_.map(_.toMetersPerSecond))
            buildDouble(x.orDefault, "WINDSPEE")
          },
          {
            val x = gwsReader.getWindVelocity.map(_.map(_.toInternationalMilesPerHour))
            buildDouble(x.orDefault, "WINDSPE2")
          },
          {
            val x = gwsReader.getWindDirection.map(_.map(_.toDegrees))
            buildDouble(x.orDefault, "WINDDIRE")
          }
        ))
        case _       => SeqAction(())
      }
    }

    override def sendAfter(id: ImageFileId): SeqAction[Unit] = SeqAction(())
  }
}
