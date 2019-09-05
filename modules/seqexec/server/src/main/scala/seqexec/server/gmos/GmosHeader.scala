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
  def header[F[_]: Sync](
    inst:              InstrumentSystem[F],
    gmosObsReader:     GmosObsKeywordsReader[F],
    gmosReader:        GmosKeywordReader[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        nsBeforeKeywords.flatMap(nsKs =>
          sendKeywords(id, inst, List(
            buildInt32(tcsKeywordsReader.gmosInstPort, KeywordName.INPORT),
            buildString(gmosReader.ccName, KeywordName.GMOSCC),
            buildString(tcsKeywordsReader.ut, KeywordName.TIME_OBS),
            buildBoolean(gmosObsReader.preimage, KeywordName.PREIMAGE, DefaultHeaderValue.FalseDefaultValue)
          ) ::: nsKs )
        )

      private def adcKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosReader.isADCInUse.ifM(
          List(
            buildDouble(gmosReader.adcPrismEntSt, KeywordName.ADCENPST),
            buildDouble(gmosReader.adcPrismEntEnd, KeywordName.ADCENPEN),
            buildDouble(gmosReader.adcPrismEntMe, KeywordName.ADCENPME),
            buildDouble(gmosReader.adcPrismExtSt, KeywordName.ADCEXPST),
            buildDouble(gmosReader.adcPrismExtEnd, KeywordName.ADCEXPEN),
            buildDouble(gmosReader.adcPrismExtMe, KeywordName.ADCEXPME),
            buildDouble(gmosReader.adcWavelength1, KeywordName.ADCWLEN1),
            buildDouble(gmosReader.adcWavelength2, KeywordName.ADCWLEN2)
          ).pure[F]
        , List.empty.pure[F])

      private def roiKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosReader.roiValues.map { _.flatMap {
          case (i, rv) =>
            List(
              KeywordName.fromTag(s"DETRO${i}X").map(buildInt32(rv.xStart.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}XS").map(buildInt32(rv.xSize.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}Y").map(buildInt32(rv.yStart.pure[F], _)),
              KeywordName.fromTag(s"DETRO${i}YS").map(buildInt32(rv.ySize.pure[F], _))
            ).flattenOption
        } }

      private def nsBeforeKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosObsReader.isNS.ifM(
          List(
            buildString(gmosObsReader.nodMode, KeywordName.NODMODE),
            buildInt32(gmosObsReader.nodPix, KeywordName.NODPIX),
            buildInt32(gmosObsReader.nodCount, KeywordName.NODCOUNT),
            buildDouble(gmosObsReader.nodAxOff, KeywordName.NODAXOFF),
            buildDouble(gmosObsReader.nodAyOff, KeywordName.NODAYOFF),
            buildDouble(gmosObsReader.nodBxOff, KeywordName.NODBXOFF),
            buildDouble(gmosObsReader.nodByOff, KeywordName.NODBYOFF)
          ).pure[F],
          List.empty.pure[F]
        )

      private def nsAfterKeywords: F[List[KeywordBag => F[KeywordBag]]] =
        gmosObsReader.isNS.ifM(
          List(
            buildInt32(gmosReader.aExpCount, KeywordName.ANODCNT),
            buildInt32(gmosReader.bExpCount, KeywordName.BNODCNT),
            buildDouble(gmosReader.exposureTime, KeywordName.SUBINT)
          ).pure[F],
          List.empty.pure[F]
        )

      private val InBeam: Int = 0
      private def readMaskName: F[String] =
        gmosReader.maskLoc
          .map(_ === InBeam)
          .ifM(gmosReader.maskName, "None".pure[F])

      def gmosKeywords(
        id:          ImageFileId,
        adcKeywords: List[KeywordBag => F[KeywordBag]],
        roiKeywords: List[KeywordBag => F[KeywordBag]],
        nsKeywords: List[KeywordBag => F[KeywordBag]]
      ): F[Unit] =
        sendKeywords(
          id,
          inst,
          List(
            buildInt32(gmosReader.maskId, KeywordName.MASKID),
            buildString(readMaskName, KeywordName.MASKNAME),
            buildInt32(gmosReader.maskType, KeywordName.MASKTYP),
            buildInt32(gmosReader.maskLoc, KeywordName.MASKLOC),
            buildString(gmosReader.filter1, KeywordName.FILTER1),
            buildInt32(gmosReader.filter1Id, KeywordName.FILTID1),
            buildString(gmosReader.filter2, KeywordName.FILTER2),
            buildInt32(gmosReader.filter2Id, KeywordName.FILTID2),
            buildString(gmosReader.grating, KeywordName.GRATING),
            buildInt32(gmosReader.gratingId, KeywordName.GRATID),
            buildDouble(gmosReader.gratingWavelength, KeywordName.GRWLEN),
            buildDouble(gmosReader.gratingAdjustedWavelength,
                         KeywordName.CENTWAVE),
            buildInt32(gmosReader.gratingOrder, KeywordName.GRORDER),
            buildDouble(gmosReader.gratingTilt, KeywordName.GRTILT),
            buildDouble(gmosReader.gratingStep, KeywordName.GRSTEP),
            buildDouble(gmosReader.dtaX, KeywordName.DTAX),
            buildDouble(gmosReader.dtaY, KeywordName.DTAY),
            buildDouble(gmosReader.dtaZ, KeywordName.DTAZ),
            buildDouble(gmosReader.dtaZst, KeywordName.DTAZST),
            buildDouble(gmosReader.dtaZen, KeywordName.DTAZEN),
            buildDouble(gmosReader.dtaZme, KeywordName.DTAZME),
            buildString(gmosReader.stageMode, KeywordName.DTMODE),
            buildString(gmosReader.adcMode, KeywordName.ADCMODE),
            buildString(gmosReader.dcName, KeywordName.GMOSDC),
            buildString(gmosReader.detectorType, KeywordName.DETTYPE),
            buildString(gmosReader.detectorId, KeywordName.DETID),
            buildDouble(
              gmosObsReader.isNS.ifM(
                (gmosObsReader.nodCount, gmosReader.exposureTime).mapN{ case (c, t) => 2 * c * t },
                gmosReader.exposureTime
              ),
              KeywordName.EXPOSURE),
            buildInt32(gmosReader.adcUsed, KeywordName.ADCUSED),
            buildInt32(gmosReader.detNRoi, KeywordName.DETNROI)
          ) ::: adcKeywords ::: roiKeywords ::: nsKeywords
        )

      override def sendAfter(id: ImageFileId): F[Unit] =
        for {
          adc <- adcKeywords
          roi <- roiKeywords
          nsK <- nsAfterKeywords
          _   <- gmosKeywords(id, adc, roi, nsK)
        } yield ()

    }
}
