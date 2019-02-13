// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.MonadError
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader

object NifsHeader {

  def header[F[_]: MonadError[?[_], Throwable]](
    inst:              InstrumentSystem[F],
    instReader:        NifsKeywordReader[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(
        id,
        inst,
        List(
          buildStringS(instReader.windowCover, KeywordName.WINDCOVR),
          buildDoubleS(instReader.maskOffset, KeywordName.APOFFSET),
          buildDoubleS(instReader.centralWavelength, KeywordName.GRATWAVE),
          buildStringS(instReader.imagingMirror, KeywordName.FLIP),
          buildDoubleS(instReader.exposureTime, KeywordName.EXPRQ),
          buildStringS(instReader.dcName, KeywordName.DCNAME),
          buildDoubleS(instReader.period, KeywordName.PERIOD),
          buildInt32S(instReader.numberOfPeriods, KeywordName.NPERIODS),
          buildStringS(instReader.exposureMode, KeywordName.EXPMODE),
          buildDoubleS(instReader.readTime, KeywordName.RDTIME),
          buildInt32S(instReader.coadds, KeywordName.COADDS),
          buildDoubleS(instReader.biasPwr, KeywordName.BIASPWR),
          buildInt32S(instReader.numberOfFowSamples, KeywordName.LNRS),
          buildStringS("IFU".pure[F], KeywordName.OBSMODE),
          buildStringS(tcsKeywordsReader.getDate, KeywordName.DATE_OBS)
        )
      )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id,
                   inst,
                   List(
                     buildDoubleS(instReader.exposureTime, KeywordName.EXPTIME)
                   ))
  }
}
