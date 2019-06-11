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
import seqexec.server.{InstrumentSystem, OcsBuildInfo, sgoEq}
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

  val obsObject: F[String] = (for {
    obsType   <- obsReader.obsType
    obsObject <- obsReader.obsObject
    tcsObject <- tcsReader.sourceATarget.objectName
  } yield if (obsType === "OBJECT" && obsObject =!= "Twilight" && obsObject =!= "Domeflat") tcsObject.some
          else obsObject.some).safeValOrDefault

  private def optTcsKeyword[B](s: TcsController.Subsystem)(v: F[B])(implicit d: DefaultHeaderValue[B]) : F[B] =
    if (tcsSubsystems.contains(s)) v.safeValOrDefault else d.default.pure[F]

  private def mountTcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.Mount)(v)

  private def m2TcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.M2)(v)

  private def sfTcsKeyword[B: DefaultHeaderValue](v: F[B]) =
    optTcsKeyword[B](TcsController.Subsystem.AGUnit)(v)

  private val baseKeywords = List(
    buildStringS(OcsBuildInfo.version.pure[F], KeywordName.SEQEXVER),
    buildStringS(obsObject, KeywordName.OBJECT),
    buildStringS(obsReader.obsType, KeywordName.OBSTYPE),
    buildStringS(obsReader.obsClass, KeywordName.OBSCLASS),
    buildStringS(obsReader.gemPrgId, KeywordName.GEMPRGID),
    buildStringS(obsReader.obsId, KeywordName.OBSID),
    buildStringS(obsReader.dataLabel, KeywordName.DATALAB),
    buildStringS(stateReader.observerName, KeywordName.OBSERVER),
    buildStringS(obsReader.observatory, KeywordName.OBSERVAT),
    buildStringS(obsReader.telescope, KeywordName.TELESCOP),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.parallax), KeywordName.PARALLAX),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.radialVelocity), KeywordName.RADVEL),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.epoch), KeywordName.EPOCH),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.equinox), KeywordName.EQUINOX),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingEquinox), KeywordName.TRKEQUIN),
    buildStringS(stateReader.operatorName, KeywordName.SSA),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.ra), KeywordName.RA),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.dec), KeywordName.DEC),
    buildDoubleS(tcsReader.elevation, KeywordName.ELEVATIO),
    buildDoubleS(tcsReader.azimuth, KeywordName.AZIMUTH),
    buildDoubleS(mountTcsKeyword(tcsReader.crPositionAngle), KeywordName.CRPA),
    buildStringS(tcsReader.hourAngle, KeywordName.HA),
    buildStringS(tcsReader.localTime, KeywordName.LT),
    buildStringS(mountTcsKeyword(tcsReader.trackingFrame), KeywordName.TRKFRAME),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingDec), KeywordName.DECTRACK),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingEpoch), KeywordName.TRKEPOCH),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingRA), KeywordName.RATRACK),
    buildStringS(mountTcsKeyword(tcsReader.sourceATarget.frame), KeywordName.FRAME),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.properMotionDec), KeywordName.PMDEC),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.properMotionRA), KeywordName.PMRA),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.wavelength), KeywordName.WAVELENG),
    buildStringS(stateReader.rawImageQuality, KeywordName.RAWIQ),
    buildStringS(stateReader.rawCloudCover, KeywordName.RAWCC),
    buildStringS(stateReader.rawWaterVapor, KeywordName.RAWWV),
    buildStringS(stateReader.rawBackgroundLight, KeywordName.RAWBG),
    buildStringS(obsReader.pIReq, KeywordName.RAWPIREQ),
    buildStringS(obsReader.geminiQA, KeywordName.RAWGEMQA),
    buildStringS(tcsReader.carouselMode, KeywordName.CGUIDMOD),
    buildStringS(tcsReader.ut, KeywordName.UT),
    buildStringS(tcsReader.date, KeywordName.DATE),
    buildStringS(m2TcsKeyword(tcsReader.m2Baffle), KeywordName.M2BAFFLE),
    buildStringS(m2TcsKeyword(tcsReader.m2CentralBaffle), KeywordName.M2CENBAF),
    buildStringS(tcsReader.st, KeywordName.ST),
    buildDoubleS(mountTcsKeyword(tcsReader.xOffset), KeywordName.XOFFSET),
    buildDoubleS(mountTcsKeyword(tcsReader.yOffset), KeywordName.YOFFSET),
    buildDoubleS(mountTcsKeyword(tcsReader.pOffset), KeywordName.POFFSET),
    buildDoubleS(mountTcsKeyword(tcsReader.qOffset), KeywordName.QOFFSET),
    buildDoubleS(mountTcsKeyword(tcsReader.raOffset), KeywordName.RAOFFSET),
    buildDoubleS(mountTcsKeyword(tcsReader.decOffset), KeywordName.DECOFFSE),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingRAOffset), KeywordName.RATRGOFF),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingDecOffset), KeywordName.DECTRGOF),
    buildDoubleS(mountTcsKeyword(tcsReader.instrumentPA), KeywordName.PA),
    buildDoubleS(mountTcsKeyword(tcsReader.instrumentAA), KeywordName.IAA),
    buildDoubleS(sfTcsKeyword(tcsReader.sfRotation), KeywordName.SFRT2),
    buildDoubleS(sfTcsKeyword(tcsReader.sfTilt), KeywordName.SFTILT),
    buildDoubleS(sfTcsKeyword(tcsReader.sfLinear), KeywordName.SFLINEAR),
    buildStringS(mountTcsKeyword(tcsReader.aoFoldName), KeywordName.AOFOLD),
    buildStringS(obsReader.pwfs1GuideS, KeywordName.PWFS1_ST),
    buildStringS(obsReader.pwfs2GuideS, KeywordName.PWFS2_ST),
    buildStringS(obsReader.oiwfsGuideS, KeywordName.OIWFS_ST),
    buildStringS(obsReader.aowfsGuideS, KeywordName.AOWFS_ST),
    buildInt32S(obsReader.sciBand, KeywordName.SCIBAND)
  )

  def timinigWindows(id: ImageFileId): F[Unit] = {
    val timingWindows = obsReader.timingWindows
    val windows = Nested(timingWindows).map {
      case (i, tw) =>
        List(
          KeywordName.fromTag(f"REQTWS${i + 1}%02d").map(buildStringS(tw.start.pure[F], _)),
          KeywordName.fromTag(f"REQTWD${i + 1}%02d").map(buildDoubleS(tw.duration.pure[F], _)),
          KeywordName.fromTag(f"REQTWN${i + 1}%02d").map(buildInt32S(tw.repeat.pure[F], _)),
          KeywordName.fromTag(f"REQTWP${i + 1}%02d").map(buildDoubleS(tw.period.pure[F], _))
        ).mapFilter(identity)
    }
    .value
    .handleError(_ => Nil)

    (timingWindows.map(_.length), windows).mapN {(l, w) =>
      val windowsCount = buildInt32S(l.pure[F], KeywordName.NUMREQTW)
      sendKeywords(id, inst, windowsCount :: w.flatten)
    }.flatten
    .handleError(_ => ()) // In case there is an error ignore it
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
          requestedConditions.get(value).map(v => buildStringS(v.pure[F], keyword))
      }.mapFilter(identity)
      sendKeywords(id, inst, requested).whenA(requested.nonEmpty)
    }
    .handleError(_ => ()) // In case there is an error ignore it
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
          airMassAngle.get(value).map(v => buildDoubleS(v.pure[F], keyword))
      }.mapFilter(identity)
      sendKeywords[F](id, inst, requested).whenA(requested.nonEmpty)
    }
    .handleError(_ => ()) // In case there is an error ignore it
  }

  override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
    def guiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader[F],
                       extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = guideWith.flatMap { g =>
      val keywords: List[KeywordBag => F[KeywordBag]] = List(
        KeywordName.fromTag(s"${baseName}ARA").map(buildDoubleS(target.ra, _)),
        KeywordName.fromTag(s"${baseName}ADEC").map(buildDoubleS(target.dec, _)),
        KeywordName.fromTag(s"${baseName}ARV").map(buildDoubleS(target.radialVelocity, _)),
        KeywordName.fromTag(s"${baseName}AWAVEL").map(buildDoubleS(target.wavelength, _)),
        KeywordName.fromTag(s"${baseName}AEPOCH").map(buildDoubleS(target.epoch, _)),
        KeywordName.fromTag(s"${baseName}AEQUIN").map(buildDoubleS(target.equinox, _)),
        KeywordName.fromTag(s"${baseName}AFRAME").map(buildStringS(target.frame, _)),
        KeywordName.fromTag(s"${baseName}AOBJEC").map(buildStringS(target.objectName, _)),
        KeywordName.fromTag(s"${baseName}APMDEC").map(buildDoubleS(target.properMotionDec, _)),
        KeywordName.fromTag(s"${baseName}APMRA").map(buildDoubleS(target.properMotionRA, _)),
        KeywordName.fromTag(s"${baseName}APARAL").map(buildDoubleS(target.parallax, _))
      ).mapFilter(identity)

      sendKeywords[F](id, inst, keywords ++ extras).whenA(g === StandardGuideOptions.Value.guide)
    }
    .handleError(_ => ()) // Errors are caught here

    def standardGuiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader[F], extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = {
      val ext = KeywordName.fromTag(s"${baseName}FOCUS").map(buildDoubleS(tcsReader.m2UserFocusOffset, _)).toList ++ extras
      guiderKeywords(guideWith, baseName, target, ext)
    }

    val oiwfsKeywords = guiderKeywords(obsReader.oiwfsGuide, "OI", tcsReader.oiwfsTarget,
      List(buildDoubleS(tcsReader.oiwfsFreq, KeywordName.OIFREQ)))

    val pwfs1Keywords = standardGuiderKeywords(obsReader.pwfs1Guide, "P1", tcsReader.pwfs1Target,
      List(buildDoubleS(tcsReader.pwfs1Freq, KeywordName.P1FREQ)))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.pwfs2Guide, "P2", tcsReader.pwfs2Target,
      List(buildDoubleS(tcsReader.pwfs2Freq, KeywordName.P2FREQ)))

    val aowfsKeywords = standardGuiderKeywords(obsReader.aowfsGuide, "AO", tcsReader.aowfsTarget, Nil)

    sendKeywords(id, inst, baseKeywords) *>
    requestedConditions(id) *>
    requestedAirMassAngle(id) *>
    timinigWindows(id) *>
    pwfs1Keywords *>
    pwfs2Keywords *>
    oiwfsKeywords *>
    aowfsKeywords
  }

  override def sendAfter(id: ImageFileId): F[Unit] = sendKeywords[F](id, inst,
    List(
      buildDoubleS(tcsReader.airMass, KeywordName.AIRMASS),
      buildDoubleS(tcsReader.startAirMass, KeywordName.AMSTART),
      buildDoubleS(tcsReader.endAirMass, KeywordName.AMEND),
      buildBooleanS(obsReader.headerPrivacy, KeywordName.PROP_MD),
      buildStringS(obsReader.proprietaryMonths, KeywordName.RELEASE)
    ))
}
