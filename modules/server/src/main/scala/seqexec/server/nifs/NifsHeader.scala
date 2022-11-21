// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.MonadError
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import lucuma.core.enums.KeywordName
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader

object NifsHeader {

  def header[F[_]: MonadError[*[_], Throwable]: Logger](
    kwClient:          KeywordsClient[F],
    instReader:        NifsKeywordReader[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(
        id,
        kwClient,
        List(
          buildString(instReader.grating, KeywordName.GRATING),
          buildString(instReader.aperture, KeywordName.APERTURE),
          buildInt32(tcsKeywordsReader.nifsInstPort, KeywordName.INPORT),
          buildString(instReader.filter, KeywordName.FILTER),
          buildString(instReader.windowCover, KeywordName.WINDCOVR),
          buildDouble(instReader.maskOffset, KeywordName.APOFFSET),
          buildDouble(instReader.centralWavelength, KeywordName.GRATWAVE),
          buildString(instReader.imagingMirror, KeywordName.FLIP),
          buildDouble(instReader.exposureTime, KeywordName.EXPRQ),
          buildString(instReader.dcName, KeywordName.DCNAME),
          buildDouble(instReader.period, KeywordName.PERIOD),
          buildInt32(instReader.numberOfPeriods, KeywordName.NPERIODS),
          buildString(instReader.exposureMode, KeywordName.EXPMODE),
          buildDouble(instReader.readTime, KeywordName.RDTIME),
          buildInt32(instReader.coadds, KeywordName.COADDS),
          buildDouble(instReader.biasPwr, KeywordName.BIASPWR),
          buildInt32(instReader.numberOfFowSamples, KeywordName.LNRS),
          buildString("IFU".pure[F], KeywordName.OBSMODE),
          buildString(tcsKeywordsReader.date, KeywordName.DATE_OBS)
        )
      )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id,
                   kwClient,
                   List(
                     buildDouble(instReader.exposureTime, KeywordName.EXPTIME)
                   )
      )
  }
}
