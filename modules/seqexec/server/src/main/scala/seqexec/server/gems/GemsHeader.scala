// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.effect.Sync
import cats.implicits._
import edu.gemini.spModel.guide.StandardGuideOptions
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords.{Header, KeywordBag, ObsKeywordsReader, buildDouble, buildInt32, buildString, sendKeywords}
import seqexec.server.tcs.{CRFollow, GemsSource, TargetKeywordsReader, TcsKeywordsReader}
import seqexec.server.tcs.TcsEpics.VirtualGemsTelescope

object GemsHeader {
  def header[F[_]: Sync](inst: InstrumentSystem[F],
                         gemsReader: GemsKeywordReader[F],
                         obsReader: ObsKeywordsReader[F],
                         tcsReader: TcsKeywordsReader[F]): Header[F] = new Header[F]{

    // Extra keywords added to GeMS target keywords. They depend on the type of guider
    private def extraGemsKeywords(baseName: String, g: GemsSource): List[KeywordBag => F[KeywordBag]] = g match {
      case GemsSource.Odgw1 => List(
        KeywordName.fromTag(s"${baseName}X").map(buildInt32(gemsReader.odgw1X, _)),
        KeywordName.fromTag(s"${baseName}Y").map(buildInt32(gemsReader.odgw1Y, _)),
        KeywordName.fromTag(s"${baseName}SIZ").map(buildInt32(gemsReader.odgwSize, _))
      ).flattenOption
      case GemsSource.Odgw2 => List(
        KeywordName.fromTag(s"${baseName}X").map(buildInt32(gemsReader.odgw2X, _)),
        KeywordName.fromTag(s"${baseName}Y").map(buildInt32(gemsReader.odgw2Y, _)),
        KeywordName.fromTag(s"${baseName}SIZ").map(buildInt32(gemsReader.odgwSize, _))
      ).flattenOption
      case GemsSource.Odgw3 => List(
        KeywordName.fromTag(s"${baseName}X").map(buildInt32(gemsReader.odgw3X, _)),
        KeywordName.fromTag(s"${baseName}Y").map(buildInt32(gemsReader.odgw3Y, _)),
        KeywordName.fromTag(s"${baseName}SIZ").map(buildInt32(gemsReader.odgwSize, _))
      ).flattenOption
      case GemsSource.Odgw4 => List(
        KeywordName.fromTag(s"${baseName}X").map(buildInt32(gemsReader.odgw4X, _)),
        KeywordName.fromTag(s"${baseName}Y").map(buildInt32(gemsReader.odgw4Y, _)),
        KeywordName.fromTag(s"${baseName}SIZ").map(buildInt32(gemsReader.odgwSize, _))
      ).flattenOption
      case _                => List.empty
    }

    private def encodeGemsSource(gs: GemsSource): String = gs.epicsVal

    private def baseName(v: VirtualGemsTelescope): String = v match {
      case VirtualGemsTelescope.G1 => "GWFS1"
      case VirtualGemsTelescope.G2 => "GWFS2"
      case VirtualGemsTelescope.G3 => "GWFS3"
      case VirtualGemsTelescope.G4 => "GWFS4"
    }

    private def gemsTargetKeyword(id: ImageFileId, v: VirtualGemsTelescope, gs: GemsSource): F[Unit] =
      guiderKeywords(id, baseName(v), tcsReader.gwfsTarget(v), gs)

    def guideWith(gs: GemsSource): F[StandardGuideOptions.Value] = gs match {
      case GemsSource.Cwfs1 => obsReader.cwfs1Guide
      case GemsSource.Cwfs2 => obsReader.cwfs2Guide
      case GemsSource.Cwfs3 => obsReader.cwfs3Guide
      case GemsSource.Odgw1 => obsReader.odgw1Guide
      case GemsSource.Odgw2 => obsReader.odgw2Guide
      case GemsSource.Odgw3 => obsReader.odgw3Guide
      case GemsSource.Odgw4 => obsReader.odgw4Guide
    }

    def guiderKeywords(id: ImageFileId, baseName: String,target: TargetKeywordsReader[F], gs: GemsSource)
    : F[Unit] = guideWith(gs).flatMap { g =>
        val keywords: List[KeywordBag => F[KeywordBag]] = extraGemsKeywords(baseName, gs) ++ List(
          KeywordName.fromTag(s"${baseName}CFG").map(buildString[F](encodeGemsSource(gs).pure[F], _)),
          KeywordName.fromTag(s"${baseName}OBJ").map(buildString(target.objectName, _)),
          KeywordName.fromTag(s"${baseName}RA").map(buildDouble(target.ra, _)),
          KeywordName.fromTag(s"${baseName}DEC").map(buildDouble(target.dec, _)),
          KeywordName.fromTag(s"${baseName}RV").map(buildDouble(target.radialVelocity, _)),
          KeywordName.fromTag(s"${baseName}EPC").map(buildDouble(target.epoch, _)),
          KeywordName.fromTag(s"${baseName}EQN").map(buildDouble(target.equinox, _)),
          KeywordName.fromTag(s"${baseName}FRM").map(buildString(target.frame, _)),
          KeywordName.fromTag(s"${baseName}PMD").map(buildDouble(target.properMotionDec, _)),
          KeywordName.fromTag(s"${baseName}PMR").map(buildDouble(target.properMotionRA, _)),
          KeywordName.fromTag(s"${baseName}PAR").map(buildDouble(target.parallax, _)),
          KeywordName.fromTag(s"${baseName}WAV").map(buildDouble(target.wavelength, _))
        ).flattenOption

        sendKeywords[F](id, inst, keywords).whenA(g.isActive)
      }.handleError(_ => ())

    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      tcsReader.gwfsMap.flatMap(_.toList.map{ case (v, gs) => gemsTargetKeyword(id, v, gs) }.sequence).void

    def cntKeywords(v: VirtualGemsTelescope, gs: GemsSource, guideOp: StandardGuideOptions.Value)
    : Option[KeywordBag => F[KeywordBag]] = {

      val cts = gs match {
        case GemsSource.Cwfs1 => gemsReader.cwfs1Counts
        case GemsSource.Cwfs2 => gemsReader.cwfs2Counts
        case GemsSource.Cwfs3 => gemsReader.cwfs3Counts
        case GemsSource.Odgw1 => gemsReader.odgw1Counts
        case GemsSource.Odgw2 => gemsReader.odgw2Counts
        case GemsSource.Odgw3 => gemsReader.odgw3Counts
        case GemsSource.Odgw4 => gemsReader.odgw4Counts
      }

      KeywordName.fromTag(s"${baseName(v)}CTS").map(buildDouble(cts, _)).filter(_ => guideOp.isActive)
    }

    override def sendAfter(id: ImageFileId): F[Unit] = {
      val keywords = List(
        buildString(tcsReader.crFollow.map(_.map(CRFollow.keywordValue).getOrElse("INDEF")), KeywordName.CRFOLLOW),
        buildString(gemsReader.sadc, KeywordName.GEMSSADC),
        buildDouble(gemsReader.dichroic, KeywordName.GEMSDICH),
        buildString(gemsReader.astrometricMode, KeywordName.GEMSASTR),
        buildString(gemsReader.nadc, KeywordName.GEMSNADC),
        buildDouble(gemsReader.lgswfs1Counts, KeywordName.LGWFS1CT),
        buildDouble(gemsReader.lgswfs2Counts, KeywordName.LGWFS2CT),
        buildDouble(gemsReader.lgswfs3Counts, KeywordName.LGWFS3CT),
        buildDouble(gemsReader.lgswfs4Counts, KeywordName.LGWFS4CT),
        buildDouble(gemsReader.lgswfs5Counts, KeywordName.LGWFS5CT),
        buildString(gemsReader.lgsLoop, KeywordName.LGSLOOP),
        buildString(gemsReader.ttLoop, KeywordName.TTLOOP),
        buildString(gemsReader.focLoop, KeywordName.FOCLOOP),
        buildString(gemsReader.flexLoop, KeywordName.FLEXLOOP),
        buildDouble(gemsReader.lgsStrhl, KeywordName.LGSSTRHL),
        buildDouble(gemsReader.rZeroVal, KeywordName.RZEROVAL),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE1),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE2),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE3),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE4),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE5),
        buildDouble(gemsReader.cnSquare1, KeywordName.CNSQARE6)
      )

      sendKeywords[F](id, inst, keywords) *>
        tcsReader.gwfsMap.flatMap{
          _.toList.map{
            case (v, gs) => guideWith(gs).map(cntKeywords(v, gs, _))
          }.sequence
        }.map(_.flattenOption).flatMap(sendKeywords(id, inst, _))

    }
  }
}
