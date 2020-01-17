// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import cats.data.Nested
import edu.gemini.spModel.guide.StandardGuideOptions
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.Conditions
import seqexec.model.{Observer, Operator}
import seqexec.model.dhs.ImageFileId
import seqexec.server.{InstrumentSystem, OcsBuildInfo}
import seqexec.server.tcs.{TargetKeywordsReader, TcsController, TcsKeywordsReader}

final case class StateKeywordsReader[F[_]: Applicative](
  conditions: Conditions,
  operator: Option[Operator],
  observer: Option[Observer]) {
  private def encodeCondition(c: Int): F[String] = (if(c === 100) "Any" else s"$c-percentile").pure[F]

  def observerName: F[String] = observer.map(_.value).filter(_.nonEmpty).getOrElse("observer").pure[F]
  def operatorName: F[String] = operator.map(_.value).filter(_.nonEmpty).getOrElse("ssa").pure[F]
  def rawImageQuality: F[String] = encodeCondition(conditions.iq.toInt)
  def rawCloudCover: F[String] = encodeCondition(conditions.cc.toInt)
  def rawWaterVapor: F[String] = encodeCondition(conditions.wv.toInt)
  def rawBackgroundLight: F[String] = encodeCondition(conditions.sb.toInt)
}

class StandardHeader[F[_]: Sync](
  inst: InstrumentSystem[F],
  obsReader: ObsKeywordsReader[F],
  tcsReader: TcsKeywordsReader[F],
  stateReader: StateKeywordsReader[F],
  tcsSubsystems: List[TcsController.Subsystem]) extends Header[F] with ObsKeywordsReaderConstants {

  val obsObject: F[String] = for {
    obsType   <- obsReader.obsType
    obsObject <- obsReader.obsObject
    tcsObject <- tcsReader.sourceATarget.objectName
  } yield if (obsType === "OBJECT" && obsObject =!= "Twilight" && obsObject =!= "Domeflat") tcsObject
          else obsObject

  private def optTcsKeyword[B](s: TcsController.Subsystem)(v: F[B])(implicit d: DefaultHeaderValue[B]) : F[B] =
    if (tcsSubsystems.contains(s)) v else d.default.pure[F]

  private def mountTcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.Mount)(v)

  private def m2TcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.M2)(v)

  private def sfTcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.AGUnit)(v)

  private val baseKeywords = List(
    buildString(OcsBuildInfo.version.pure[F], KeywordName.SEQEXVER),
    buildString(obsObject, KeywordName.OBJECT),
    buildString(obsReader.obsType, KeywordName.OBSTYPE),
    buildString(obsReader.obsClass, KeywordName.OBSCLASS),
    buildString(obsReader.gemPrgId, KeywordName.GEMPRGID),
    buildString(obsReader.obsId, KeywordName.OBSID),
    buildString(obsReader.dataLabel, KeywordName.DATALAB),
    buildString(stateReader.observerName, KeywordName.OBSERVER),
    buildString(obsReader.observatory, KeywordName.OBSERVAT),
    buildString(obsReader.telescope, KeywordName.TELESCOP),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.parallax), KeywordName.PARALLAX),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.radialVelocity), KeywordName.RADVEL),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.epoch), KeywordName.EPOCH),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.equinox), KeywordName.EQUINOX),
    buildDouble(mountTcsKeyword(tcsReader.trackingEquinox), KeywordName.TRKEQUIN),
    buildString(stateReader.operatorName, KeywordName.SSA),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.ra), KeywordName.RA),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.dec), KeywordName.DEC),
    buildDouble(tcsReader.elevation, KeywordName.ELEVATIO),
    buildDouble(tcsReader.azimuth, KeywordName.AZIMUTH),
    buildDouble(tcsReader.crPositionAngle, KeywordName.CRPA),
    buildString(tcsReader.hourAngle, KeywordName.HA),
    buildString(tcsReader.localTime, KeywordName.LT),
    buildString(mountTcsKeyword(tcsReader.trackingFrame), KeywordName.TRKFRAME),
    buildDouble(mountTcsKeyword(tcsReader.trackingDec), KeywordName.DECTRACK),
    buildDouble(mountTcsKeyword(tcsReader.trackingEpoch), KeywordName.TRKEPOCH),
    buildDouble(mountTcsKeyword(tcsReader.trackingRA), KeywordName.RATRACK),
    buildString(mountTcsKeyword(tcsReader.sourceATarget.frame), KeywordName.FRAME),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.properMotionDec), KeywordName.PMDEC),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.properMotionRA), KeywordName.PMRA),
    buildDouble(mountTcsKeyword(tcsReader.sourceATarget.wavelength), KeywordName.WAVELENG),
    buildString(stateReader.rawImageQuality, KeywordName.RAWIQ),
    buildString(stateReader.rawCloudCover, KeywordName.RAWCC),
    buildString(stateReader.rawWaterVapor, KeywordName.RAWWV),
    buildString(stateReader.rawBackgroundLight, KeywordName.RAWBG),
    buildString(obsReader.pIReq, KeywordName.RAWPIREQ),
    buildString(obsReader.geminiQA, KeywordName.RAWGEMQA),
    buildString(tcsReader.carouselMode, KeywordName.CGUIDMOD),
    buildString(tcsReader.ut, KeywordName.UT),
    buildString(tcsReader.date, KeywordName.DATE),
    buildString(m2TcsKeyword(tcsReader.m2Baffle), KeywordName.M2BAFFLE),
    buildString(m2TcsKeyword(tcsReader.m2CentralBaffle), KeywordName.M2CENBAF),
    buildString(tcsReader.st, KeywordName.ST),
    buildDouble(mountTcsKeyword(tcsReader.xOffset), KeywordName.XOFFSET),
    buildDouble(mountTcsKeyword(tcsReader.yOffset), KeywordName.YOFFSET),
    buildDouble(mountTcsKeyword(tcsReader.pOffset), KeywordName.POFFSET),
    buildDouble(mountTcsKeyword(tcsReader.qOffset), KeywordName.QOFFSET),
    buildDouble(mountTcsKeyword(tcsReader.raOffset), KeywordName.RAOFFSET),
    buildDouble(mountTcsKeyword(tcsReader.decOffset), KeywordName.DECOFFSE),
    buildDouble(mountTcsKeyword(tcsReader.trackingRAOffset), KeywordName.RATRGOFF),
    buildDouble(mountTcsKeyword(tcsReader.trackingDecOffset), KeywordName.DECTRGOF),
    buildDouble(mountTcsKeyword(tcsReader.instrumentPA), KeywordName.PA),
    buildDouble(mountTcsKeyword(tcsReader.instrumentAA), KeywordName.IAA),
    buildDouble(sfTcsKeyword(tcsReader.sfRotation), KeywordName.SFRT2),
    buildDouble(sfTcsKeyword(tcsReader.sfTilt), KeywordName.SFTILT),
    buildDouble(sfTcsKeyword(tcsReader.sfLinear), KeywordName.SFLINEAR),
    buildString(mountTcsKeyword(tcsReader.aoFoldName), KeywordName.AOFOLD),
    buildString(obsReader.pwfs1GuideS, KeywordName.PWFS1_ST),
    buildString(obsReader.pwfs2GuideS, KeywordName.PWFS2_ST),
    buildString(obsReader.oiwfsGuideS, KeywordName.OIWFS_ST),
    buildString(obsReader.aowfsGuideS, KeywordName.AOWFS_ST),
    buildInt32(obsReader.sciBand, KeywordName.SCIBAND)
  )

  def timingWindows(id: ImageFileId): F[Unit] = {
    val timingWindows = obsReader.timingWindows
    val windows = Nested(timingWindows).map {
      case (i, tw) =>
        List(
          KeywordName.fromTag(f"REQTWS${i + 1}%02d").map(buildString(tw.start.pure[F], _)),
          KeywordName.fromTag(f"REQTWD${i + 1}%02d").map(buildDouble(tw.duration.pure[F], _)),
          KeywordName.fromTag(f"REQTWN${i + 1}%02d").map(buildInt32(tw.repeat.pure[F], _)),
          KeywordName.fromTag(f"REQTWP${i + 1}%02d").map(buildDouble(tw.period.pure[F], _))
        ).mapFilter(identity)
    }
    .value

    (timingWindows.map(_.length), windows).mapN {(l, w) =>
      val windowsCount = buildInt32(l.pure[F], KeywordName.NUMREQTW)
      sendKeywords(id, inst, windowsCount :: w.flatten)
    }.flatten
  }

  // TODO abstract requestedConditions/requestedAirMassAngle
  def requestedConditions(id: ImageFileId): F[Unit] = {
    val keys = List(
      KeywordName.REQIQ -> IQ,
      KeywordName.REQCC -> CC,
      KeywordName.REQBG -> SB,
      KeywordName.REQWV -> WV)
    obsReader.requestedConditions.flatMap { requestedConditions =>
      val requested = keys.map {
        case (keyword, value) =>
          requestedConditions.get(value).map(v => buildString(v.pure[F], keyword))
      }.mapFilter(identity)
      sendKeywords(id, inst, requested).whenA(requested.nonEmpty)
    }
  }

  def requestedAirMassAngle(id: ImageFileId): F[Unit] = {
    val keys = List(
      KeywordName.REQMAXAM -> MAX_AIRMASS,
      KeywordName.REQMAXHA -> MAX_HOUR_ANGLE,
      KeywordName.REQMINAM -> MIN_AIRMASS,
      KeywordName.REQMINHA -> MIN_HOUR_ANGLE)
    obsReader.requestedAirMassAngle.flatMap { airMassAngle =>
      val requested = keys.map {
        case (keyword, value) =>
          airMassAngle.get(value).map(v => buildDouble(v.pure[F], keyword))
      }.mapFilter(identity)
      sendKeywords[F](id, inst, requested).whenA(requested.nonEmpty)
    }
  }


  def guiderKeywords(id: ImageFileId, guideWith: F[StandardGuideOptions.Value], baseName: String,
                     target: TargetKeywordsReader[F], extras: List[KeywordBag => F[KeywordBag]])
  : F[Unit] = guideWith.flatMap { g =>
    val keywords: List[KeywordBag => F[KeywordBag]] = List(
      KeywordName.fromTag(s"${baseName}ARA").map(buildDouble(target.ra, _)),
      KeywordName.fromTag(s"${baseName}ADEC").map(buildDouble(target.dec, _)),
      KeywordName.fromTag(s"${baseName}ARV").map(buildDouble(target.radialVelocity, _)),
      KeywordName.fromTag(s"${baseName}AWAVEL").map(buildDouble(target.wavelength, _)),
      KeywordName.fromTag(s"${baseName}AEPOCH").map(buildDouble(target.epoch, _)),
      KeywordName.fromTag(s"${baseName}AEQUIN").map(buildDouble(target.equinox, _)),
      KeywordName.fromTag(s"${baseName}AFRAME").map(buildString(target.frame, _)),
      KeywordName.fromTag(s"${baseName}AOBJEC").map(buildString(target.objectName, _)),
      KeywordName.fromTag(s"${baseName}APMDEC").map(buildDouble(target.properMotionDec, _)),
      KeywordName.fromTag(s"${baseName}APMRA").map(buildDouble(target.properMotionRA, _)),
      KeywordName.fromTag(s"${baseName}APARAL").map(buildDouble(target.parallax, _))
    ).mapFilter(identity)

    sendKeywords[F](id, inst, keywords ++ extras).whenA(g.isActive)
  }
  .handleError(_ => ()) // Errors on guideWith are caught here

  def standardGuiderKeywords(id: ImageFileId, guideWith: F[StandardGuideOptions.Value], baseName: String,
                             target: TargetKeywordsReader[F], extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = {
    val ext = KeywordName.fromTag(s"${baseName}FOCUS").map(buildDouble(tcsReader.m2UserFocusOffset, _)).toList ++ extras
    guiderKeywords(id, guideWith, baseName, target, ext)
  }

  override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
    val oiwfsKeywords = guiderKeywords(id, obsReader.oiwfsGuide, "OI", tcsReader.oiwfsTarget,
      List(buildDouble(tcsReader.oiwfsFreq, KeywordName.OIFREQ)))

    val pwfs1Keywords = standardGuiderKeywords(id, obsReader.pwfs1Guide, "P1", tcsReader.pwfs1Target,
      List(buildDouble(tcsReader.pwfs1Freq, KeywordName.P1FREQ)))

    val pwfs2Keywords = standardGuiderKeywords(id, obsReader.pwfs2Guide, "P2", tcsReader.pwfs2Target,
      List(buildDouble(tcsReader.pwfs2Freq, KeywordName.P2FREQ)))

    val aowfsKeywords = standardGuiderKeywords(id, obsReader.aowfsGuide, "AO", tcsReader.aowfsTarget, Nil)

    sendKeywords(id, inst, baseKeywords) *>
      requestedConditions(id) *>
      requestedAirMassAngle(id) *>
      timingWindows(id) *>
      pwfs1Keywords *>
      pwfs2Keywords *>
      oiwfsKeywords *>
      aowfsKeywords
  }

  override def sendAfter(id: ImageFileId): F[Unit] = sendKeywords[F](id, inst,
    List(
      buildDouble(tcsReader.airMass, KeywordName.AIRMASS),
      buildDouble(tcsReader.startAirMass, KeywordName.AMSTART),
      buildDouble(tcsReader.endAirMass, KeywordName.AMEND),
      buildBoolean(obsReader.headerPrivacy, KeywordName.PROP_MD, DefaultHeaderValue.FalseDefaultValue),
      buildString(obsReader.proprietaryMonths, KeywordName.RELEASE)
    ))
}
