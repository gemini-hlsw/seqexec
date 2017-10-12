// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gws

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.Header.Implicits._
import edu.gemini.seqexec.server.Header._
import edu.gemini.seqexec.server.{DhsClient, EpicsHealth, Header, SeqAction}

/**
  * Created by jluhrs on 7/17/17.
  */
class GwsHeader(hs: DhsClient, gwsReader: GwsKeywordReader) extends Header {
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] = {
    gwsReader.getHealth.flatMap{
      case Some(EpicsHealth.Good) => sendKeywords(id, inst, hs, List(
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

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction(())
}
