// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader

object GsaoiHeader extends GsaoiLUT {

  def header[F[_]: Sync](
    inst:              InstrumentSystem[F],
    tcsKeywordsReader: TcsKeywordsReader[F],
    instReader:        GsaoiKeywordReader[F]
  ): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(
        id,
        inst,
        List(
          buildInt32S(tcsKeywordsReader.getGsaoiInstPort, KeywordName.INPORT),
          buildStringS(instReader.upperFilter, KeywordName.FILTER1),
          buildInt32S(instReader.upperFilterEngPos, KeywordName.FILT1POS),
          buildStringS(instReader.upperFilterHealth, KeywordName.FILT1CAR),
          buildStringS(instReader.lowerFilter, KeywordName.FILTER2),
          buildInt32S(instReader.lowerFilterEngPos, KeywordName.FILT2POS),
          buildStringS(instReader.lowerFilterHealth, KeywordName.FILT2CAR),
          buildStringS(instReader.utilityWheel, KeywordName.UTLWHEEL),
          buildInt32S(instReader.utilityWheelEngPos, KeywordName.UTLWPOS),
          buildStringS(instReader.utilityWheelHealth, KeywordName.UTLWCAR),
          buildStringS(instReader.windowCover, KeywordName.COVER),
          buildInt32S(instReader.windowCoverEngPos, KeywordName.CVERPOS),
          buildStringS(instReader.windowCoverHealth, KeywordName.CVERCAR),
          buildDoubleS(instReader.coldworkSurfaceTemperature, KeywordName.CWSTEMP),
          buildDoubleS(instReader.detectorTemperature, KeywordName.DETTEMP),
          buildDoubleS(instReader.detectorHousingTemperature, KeywordName.DETHTEMP),
          buildStringS(instReader.dewarPressure, KeywordName.DEWPRES),
          buildStringS(instReader.dateObs, KeywordName.DATE_OBS),
          buildStringS(tcsKeywordsReader.getUT, KeywordName.TIME_OBS),
          buildDoubleS(instReader.mjdobs, KeywordName.MJD_OBS),
          buildStringS(instReader.readMode, KeywordName.SAMPMODE),
          buildStringS(instReader.expositionMode, KeywordName.EXPMODE),
          buildInt32S(instReader.numberOfResets, KeywordName.NRESETS),
          buildDoubleS(instReader.resetDelay, KeywordName.RSTDLAY),
          buildDoubleS(instReader.readTime, KeywordName.READTIME),
          buildStringS(instReader.bUnits, KeywordName.BUNIT),
          buildStringS(instReader.dcName, KeywordName.DCNAME),
          buildStringS(instReader.dcHealth, KeywordName.DCHLTH),
          buildStringS(instReader.simulationMode, KeywordName.DCSIM),
          buildStringS(instReader.timingBoardCodeName, KeywordName.DSPTIMBN),
          buildStringS(instReader.dspCodeVersion, KeywordName.DSPTIMBV),
          buildStringS(instReader.pciBoardCodeName, KeywordName.DSPPCIN),
          buildStringS(instReader.dspCodeVersion, KeywordName.DSPPCIV)
        )
      )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(
        id,
        inst,
        List(
          buildDoubleS(instReader.obsElapsedTime, KeywordName.ELAPSED),
          buildDoubleS(instReader.readInterval, KeywordName.READDLAY)
        ))
  }
}
