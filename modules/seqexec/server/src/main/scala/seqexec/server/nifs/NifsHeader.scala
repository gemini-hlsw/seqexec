// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.MonadError
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import scala.math.cos
import scala.math.sin
import scala.math.toRadians
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
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
      // position angle in radians
      val φ = tcsKeywordsReader.getInstrumentPA.map(toRadians)
      sendKeywords(
        id,
        inst,
        List(
          buildStringS(instReader.grating, KeywordName.GRATING),
          buildStringS(instReader.aperture, KeywordName.APERTURE),
          buildInt32S(tcsKeywordsReader.getNifsInstPort, KeywordName.INPORT),
          buildStringS(instReader.filter, KeywordName.FILTER),
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
          buildStringS(tcsKeywordsReader.getDate, KeywordName.DATE_OBS),
          // Approximate WCS
          buildStringS("RA---TAN".pure[F], KeywordName.CTYPE1),
          buildDoubleS(15.0.pure[F], KeywordName.CRPIX1),
          buildDoubleS(tcsKeywordsReader.getSourceATarget.getRA, KeywordName.CRVAL1),
          buildStringS("DEC--TAN".pure[F], KeywordName.CTYPE2),
          buildDoubleS(34.0.pure[F], KeywordName.CRPIX2),
          buildDoubleS(tcsKeywordsReader.getSourceATarget.getDec, KeywordName.CRVAL2),
          buildDoubleS(φ.map(φ => -4.7e-5 * sin(φ)), KeywordName.CD1_1),
          buildDoubleS(φ.map(φ =>  1.9e-5 * cos(φ)), KeywordName.CD1_2),
          buildDoubleS(φ.map(φ => -4.7e-5 * cos(φ)), KeywordName.CD2_1),
          buildDoubleS(φ.map(φ => -1.9e-5 * sin(φ)), KeywordName.CD2_2),
          buildStringS("FK5".pure[F], KeywordName.RADECSYS)
        )
      )
    }

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id,
                   inst,
                   List(
                     buildDoubleS(instReader.exposureTime, KeywordName.EXPTIME)
                   ))
  }
}
