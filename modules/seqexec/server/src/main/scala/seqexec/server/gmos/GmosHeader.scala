// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader

object GmosHeader {
  // scalastyle:off
  def header[F[_]: Sync](
    inst:              InstrumentSystem[F],
    gmosObsReader:     GmosObsKeywordsReader[F],
    gmosReader:        GmosKeywordReader[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        sendKeywords(id, inst, List(
          buildInt32S(tcsKeywordsReader.gmosInstPort, KeywordName.INPORT),
          buildStringS(gmosReader.ccName, KeywordName.GMOSCC),
          buildStringS(tcsKeywordsReader.ut, KeywordName.TIME_OBS),
          buildBooleanS(gmosObsReader.preimage, KeywordName.PREIMAGE))
          // TODO NOD*
        )

      private def adcKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosReader.isADCInUse.ifM(
          List(
            buildDoubleS(gmosReader.adcPrismEntSt, KeywordName.ADCENPST),
            buildDoubleS(gmosReader.adcPrismEntEnd, KeywordName.ADCENPEN),
            buildDoubleS(gmosReader.adcPrismEntMe, KeywordName.ADCENPME),
            buildDoubleS(gmosReader.adcPrismExtSt, KeywordName.ADCEXPST),
            buildDoubleS(gmosReader.adcPrismExtEnd, KeywordName.ADCEXPEN),
            buildDoubleS(gmosReader.adcPrismExtMe, KeywordName.ADCEXPME),
            buildDoubleS(gmosReader.adcWavelength1, KeywordName.ADCWLEN1),
            buildDoubleS(gmosReader.adcWavelength2, KeywordName.ADCWLEN2)
          ).pure[F]
        , List.empty.pure[F])

      private def roiKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosReader.roiValues.map { _.map {
          case (i, rv) =>
            List(
              KeywordName.fromTag(s"DETRO${i}X").map(buildInt32S(rv.xStart.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}XS").map(buildInt32S(rv.xSize.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}Y").map(buildInt32S(rv.yStart.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}YS").map(buildInt32S(rv.ySize.pure[F], _))
            ).mapFilter(identity)
        }.flatten}

      private val InBeam: Int = 0
      private def readMaskName: F[String] =
        gmosReader.maskLoc
          .map(_ === InBeam)
          .ifM(gmosReader.maskName, "None".pure[F])

      def gmosKeywords(
        id:          ImageFileId,
        adcKeywords: List[KeywordBag => F[KeywordBag]],
        roiKeywords: List[KeywordBag => F[KeywordBag]]
      ): F[Unit] =
        sendKeywords(
          id,
          inst,
          List(
            buildInt32S(gmosReader.maskId, KeywordName.MASKID),
            buildStringS(readMaskName, KeywordName.MASKNAME),
            buildInt32S(gmosReader.maskType, KeywordName.MASKTYP),
            buildInt32S(gmosReader.maskLoc, KeywordName.MASKLOC),
            buildStringS(gmosReader.filter1, KeywordName.FILTER1),
            buildInt32S(gmosReader.filter1Id, KeywordName.FILTID1),
            buildStringS(gmosReader.filter2, KeywordName.FILTER2),
            buildInt32S(gmosReader.filter2Id, KeywordName.FILTID2),
            buildStringS(gmosReader.grating, KeywordName.GRATING),
            buildInt32S(gmosReader.gratingId, KeywordName.GRATID),
            buildDoubleS(gmosReader.gratingWavelength, KeywordName.GRWLEN),
            buildDoubleS(gmosReader.gratingAdjustedWavelength,
                         KeywordName.CENTWAVE),
            buildInt32S(gmosReader.gratingOrder, KeywordName.GRORDER),
            buildDoubleS(gmosReader.gratingTilt, KeywordName.GRTILT),
            buildDoubleS(gmosReader.gratingStep, KeywordName.GRSTEP),
            buildDoubleS(gmosReader.dtaX, KeywordName.DTAX),
            buildDoubleS(gmosReader.dtaY, KeywordName.DTAY),
            buildDoubleS(gmosReader.dtaZ, KeywordName.DTAZ),
            buildDoubleS(gmosReader.dtaZst, KeywordName.DTAZST),
            buildDoubleS(gmosReader.dtaZen, KeywordName.DTAZEN),
            buildDoubleS(gmosReader.dtaZme, KeywordName.DTAZME),
            buildStringS(gmosReader.stageMode, KeywordName.DTMODE),
            buildStringS(gmosReader.adcMode, KeywordName.ADCMODE),
            buildStringS(gmosReader.dcName, KeywordName.GMOSDC),
            buildStringS(gmosReader.detectorType, KeywordName.DETTYPE),
            buildStringS(gmosReader.detectorId, KeywordName.DETID),
            buildDoubleS(gmosReader.exposureTime, KeywordName.EXPOSURE),
            buildInt32S(gmosReader.adcUsed, KeywordName.ADCUSED),
            buildInt32S(gmosReader.detNRoi, KeywordName.DETNROI)
            // TODO These are enabled on N&S only
            /*buildInt32S(gmosReader.aExpCount, "ANODCNT"),
            buildInt32S(gmosReader.bExpCount, "BNODCNT"),
            buildInt32S(gmosReader.exposureTime, "SUBINT")*/
          ) ::: adcKeywords ::: roiKeywords
        )

      override def sendAfter(id: ImageFileId): F[Unit] =
        for {
          adc <- adcKeywords
          roi <- roiKeywords
          _   <- gmosKeywords(id, adc, roi)
        } yield ()

    }
  // scalastyle:on
}
