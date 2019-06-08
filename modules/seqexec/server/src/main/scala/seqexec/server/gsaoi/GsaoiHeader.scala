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
          buildInt32(tcsKeywordsReader.gsaoiInstPort, KeywordName.INPORT),
          buildString(instReader.upperFilter, KeywordName.FILTER1),
          buildInt32(instReader.upperFilterEngPos, KeywordName.FILT1POS),
          buildString(instReader.upperFilterHealth, KeywordName.FILT1CAR),
          buildString(instReader.lowerFilter, KeywordName.FILTER2),
          buildInt32(instReader.lowerFilterEngPos, KeywordName.FILT2POS),
          buildString(instReader.lowerFilterHealth, KeywordName.FILT2CAR),
          buildString(instReader.utilityWheel, KeywordName.UTLWHEEL),
          buildInt32(instReader.utilityWheelEngPos, KeywordName.UTLWPOS),
          buildString(instReader.utilityWheelHealth, KeywordName.UTLWCAR),
          buildString(instReader.windowCover, KeywordName.COVER),
          buildInt32(instReader.windowCoverEngPos, KeywordName.CVERPOS),
          buildString(instReader.windowCoverHealth, KeywordName.CVERCAR),
          buildDouble(instReader.coldworkSurfaceTemperature, KeywordName.CWSTEMP),
          buildDouble(instReader.detectorTemperature, KeywordName.DETTEMP),
          buildDouble(instReader.detectorHousingTemperature, KeywordName.DETHTEMP),
          buildDouble(instReader.dewarPressure, KeywordName.DEWPRES),
          buildString(instReader.dateObs, KeywordName.DATE_OBS),
          buildString(tcsKeywordsReader.ut, KeywordName.TIME_OBS),
          buildDouble(instReader.mjdobs, KeywordName.GSAOI_MJD_OBS),
          buildString(instReader.readMode, KeywordName.SAMPMODE),
          buildString(instReader.expositionMode, KeywordName.EXPMODE),
          buildInt32(instReader.numberOfResets, KeywordName.NRESETS),
          buildDouble(instReader.resetDelay, KeywordName.RSTDLAY),
          buildDouble(instReader.readTime, KeywordName.READTIME),
          buildString(instReader.bUnits, KeywordName.BUNIT),
          buildString(instReader.dcName, KeywordName.DCNAME),
          buildString(instReader.dcHealth, KeywordName.DCHLTH),
          buildString(instReader.simulationMode, KeywordName.DCSIM),
          buildString(instReader.timingBoardCodeName, KeywordName.DSPTIMBN),
          buildString(instReader.dspCodeVersion, KeywordName.DSPTIMBV),
          buildString(instReader.pciBoardCodeName, KeywordName.DSPPCIN),
          buildString(instReader.dspCodeVersion, KeywordName.DSPPCIV)
        )
      )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(
        id,
        inst,
        List(
          buildDouble(instReader.obsElapsedTime, KeywordName.ELAPSED),
          buildDouble(instReader.readInterval, KeywordName.READDLAY)
        ))
  }
}
