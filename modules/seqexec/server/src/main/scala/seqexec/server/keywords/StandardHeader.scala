// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.Applicative
import cats.data.EitherT
import cats.implicits._
import cats.effect.Sync
import edu.gemini.spModel.guide.StandardGuideOptions
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.Conditions
import seqexec.model.{Observer, Operator}
import seqexec.model.dhs.ImageFileId
import seqexec.server.{InstrumentSystem, OcsBuildInfo, SeqActionF, sgoEq}
import seqexec.server.tcs.{TargetKeywordsReader, TcsController, TcsKeywordsReader}

// TODO: Replace Unit by something that can read the state for real
final case class StateKeywordsReader[F[_]: Sync](conditions: Conditions, operator: Option[Operator], observer: Option[Observer]) {
  def encodeCondition(c: Int): String = if(c === 100) "Any" else s"$c-percentile"

  // TODO: "observer" should be the default when not set in state
  def getObserverName: SeqActionF[F, String] = SeqActionF(observer.map(_.value).filter(_.nonEmpty).getOrElse("observer"))
  def getOperatorName: SeqActionF[F, String] = SeqActionF(operator.map(_.value).filter(_.nonEmpty).getOrElse("ssa"))
  def getRawImageQuality: SeqActionF[F, String] = SeqActionF(encodeCondition(conditions.iq.toInt))
  def getRawCloudCover: SeqActionF[F, String] = SeqActionF(encodeCondition(conditions.cc.toInt))
  def getRawWaterVapor: SeqActionF[F, String] = SeqActionF(encodeCondition(conditions.wv.toInt))
  def getRawBackgroundLight: SeqActionF[F, String] = SeqActionF(encodeCondition(conditions.sb.toInt))
}

class StandardHeader[F[_]: Sync](
  inst: InstrumentSystem[F],
  obsReader: ObsKeywordsReader[F],
  tcsReader: TcsKeywordsReader[F],
  stateReader: StateKeywordsReader[F],
  tcsSubsystems: List[TcsController.Subsystem]) extends Header[F] {

  val obsObject: SeqActionF[F, Option[String]] = for {
    obsType   <- obsReader.getObsType
    obsObject <- obsReader.getObsObject
    tcsObject <- EitherT.liftF(tcsReader.sourceATarget.objectName)
  } yield if (obsType === "OBJECT" && obsObject =!= "Twilight" && obsObject =!= "Domeflat") Some(tcsObject)
          else Some(obsObject)

  private def decodeGuide(v: StandardGuideOptions.Value): String = v match {
    case StandardGuideOptions.Value.park   => "parked"
    case StandardGuideOptions.Value.guide  => "guiding"
    case StandardGuideOptions.Value.freeze => "frozen"
  }

  private def optTcsKeyword[B](s: TcsController.Subsystem)(v: F[B])(implicit d: DefaultHeaderValue[B]) : F[B] =
    if(tcsSubsystems.contains(s)) v
    else d.default.pure[F]

  private def mountTcsKeyword[B](v: F[B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.Mount)(v)(d)

  private def m2TcsKeyword[B](v: F[B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.M2)(v)(d)

  private def sfTcsKeyword[B](v: F[B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.AGUnit)(v)(d)

  private val baseKeywords = List(
    buildString(SeqActionF(OcsBuildInfo.version), KeywordName.SEQEXVER),
    buildString(obsObject.orDefault, KeywordName.OBJECT),
    buildString(obsReader.getObsType, KeywordName.OBSTYPE),
    buildString(obsReader.getObsClass, KeywordName.OBSCLASS),
    buildString(obsReader.getGemPrgId, KeywordName.GEMPRGID),
    buildString(obsReader.getObsId, KeywordName.OBSID),
    buildString(obsReader.getDataLabel, KeywordName.DATALAB),
    buildString(stateReader.getObserverName, KeywordName.OBSERVER),
    buildString(obsReader.getObservatory, KeywordName.OBSERVAT),
    buildString(obsReader.getTelescope, KeywordName.TELESCOP),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.parallax), KeywordName.PARALLAX),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.radialVelocity), KeywordName.RADVEL),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.epoch), KeywordName.EPOCH),
    buildDoubleS(mountTcsKeyword(tcsReader.sourceATarget.equinox), KeywordName.EQUINOX),
    buildDoubleS(mountTcsKeyword(tcsReader.trackingEquinox), KeywordName.TRKEQUIN),
    buildString(stateReader.getOperatorName, KeywordName.SSA),
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
    buildString(stateReader.getRawImageQuality, KeywordName.RAWIQ),
    buildString(stateReader.getRawCloudCover, KeywordName.RAWCC),
    buildString(stateReader.getRawWaterVapor, KeywordName.RAWWV),
    buildString(stateReader.getRawBackgroundLight, KeywordName.RAWBG),
    buildString(obsReader.getPIReq, KeywordName.RAWPIREQ),
    buildString(obsReader.getGeminiQA, KeywordName.RAWGEMQA),
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
    buildString(obsReader.getPwfs1Guide.map(decodeGuide), KeywordName.PWFS1_ST),
    buildString(obsReader.getPwfs2Guide.map(decodeGuide), KeywordName.PWFS2_ST),
    buildString(obsReader.getOiwfsGuide.map(decodeGuide), KeywordName.OIWFS_ST),
    buildString(obsReader.getAowfsGuide.map(decodeGuide), KeywordName.AOWFS_ST),
    buildInt32(obsReader.getSciBand.orDefault, KeywordName.SCIBAND)
  )

  def timinigWindows(id: ImageFileId): F[Unit] = {
    val timingWindows = obsReader.getTimingWindows
    val windows = timingWindows.flatMap {
      case (i, tw) =>
        List(
          KeywordName.fromTag(f"REQTWS${i + 1}%02d").map(buildString(tw.start, _)),
          KeywordName.fromTag(f"REQTWD${i + 1}%02d").map(buildDouble(tw.duration, _)),
          KeywordName.fromTag(f"REQTWN${i + 1}%02d").map(buildInt32(tw.repeat, _)),
          KeywordName.fromTag(f"REQTWP${i + 1}%02d").map(buildDouble(tw.period, _))
        ).collect { case Some(k) => k }
    }
    val windowsCount = buildInt32(SeqActionF(timingWindows.length), KeywordName.NUMREQTW)
    sendKeywords(id, inst, windowsCount :: windows)
  }

  def requestedConditions(id: ImageFileId): F[Unit] = {
    import ObsKeywordsReader._
    val keys = List(
      KeywordName.REQIQ -> IQ,
      KeywordName.REQCC -> CC,
      KeywordName.REQBG -> SB,
      KeywordName.REQWV -> WV)
    val requested = keys.flatMap {
      case (keyword, value) => obsReader.getRequestedConditions.get(value).toList.map(buildString(_, keyword))
    }
    sendKeywords(id, inst, requested)
  }

  def requestedAirMassAngle(id: ImageFileId): F[Unit] = {
    import ObsKeywordsReader._
    val keys = List(
      KeywordName.REQMAXAM -> MAX_AIRMASS,
      KeywordName.REQMAXHA -> MAX_HOUR_ANGLE,
      KeywordName.REQMINAM -> MIN_AIRMASS,
      KeywordName.REQMINHA -> MIN_HOUR_ANGLE)
    val requested = keys.flatMap {
      case (keyword, value) => obsReader.getRequestedAirMassAngle.get(value).toList.map(buildDouble(_, keyword))
    }

    sendKeywords[F](id, inst, requested).whenA(requested.nonEmpty)
  }

  override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
    def guiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader[F],
                       extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = guideWith.flatMap { g =>
      val keywords: List[KeywordBag => F[KeywordBag]] = List(
        KeywordName.fromTag(baseName + "ARA").map(buildDoubleS(target.ra, _)),
        KeywordName.fromTag(baseName + "ADEC").map(buildDoubleS(target.dec, _)),
        KeywordName.fromTag(baseName + "ARV").map(buildDoubleS(target.radialVelocity, _)),
        KeywordName.fromTag(baseName + "AWAVEL").map(buildDoubleS(target.wavelength, _)),
        KeywordName.fromTag(baseName + "AEPOCH").map(buildDoubleS(target.epoch, _)),
        KeywordName.fromTag(baseName + "AEQUIN").map(buildDoubleS(target.equinox, _)),
        KeywordName.fromTag(baseName + "AFRAME").map(buildStringS(target.frame, _)),
        KeywordName.fromTag(baseName + "AOBJEC").map(buildStringS(target.objectName, _)),
        KeywordName.fromTag(baseName + "APMDEC").map(buildDoubleS(target.properMotionDec, _)),
        KeywordName.fromTag(baseName + "APMRA").map(buildDoubleS(target.properMotionRA, _)),
        KeywordName.fromTag(baseName + "APARAL").map(buildDoubleS(target.parallax, _))
      ).collect { case Some(k) => k }

      if (g === StandardGuideOptions.Value.guide) sendKeywords[F](id, inst, keywords ++ extras)
      else Applicative[F].unit
    }

    def standardGuiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader[F], extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = {
      val ext = KeywordName.fromTag(baseName + "FOCUS").map(buildDoubleS(tcsReader.m2UserFocusOffset, _)).toList ++ extras
      guiderKeywords(guideWith, baseName, target, ext)
    }

    val oiwfsKeywords = guiderKeywords(obsReader.getOiwfsGuide.liftF, "OI", tcsReader.oiwfsTarget,
      List(buildDoubleS(tcsReader.oiwfsFreq, KeywordName.OIFREQ)))

    val pwfs1Keywords = standardGuiderKeywords(obsReader.getPwfs1Guide.liftF, "P1", tcsReader.pwfs1Target,
      List(buildDoubleS(tcsReader.pwfs1Freq, KeywordName.P1FREQ)))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.getPwfs2Guide.liftF, "P2", tcsReader.pwfs2Target,
      List(buildDoubleS(tcsReader.pwfs2Freq, KeywordName.P2FREQ)))

    val aowfsKeywords = standardGuiderKeywords(obsReader.getAowfsGuide.liftF, "AO", tcsReader.aowfsTarget, Nil)

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
      buildBoolean(obsReader.getHeaderPrivacy, KeywordName.PROP_MD),
      buildString(obsReader.getProprietaryMonths, KeywordName.RELEASE)
    ))
}
