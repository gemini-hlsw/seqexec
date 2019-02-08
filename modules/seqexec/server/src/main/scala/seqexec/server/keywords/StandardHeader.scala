// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality._
import edu.gemini.spModel.guide.StandardGuideOptions
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.Conditions
import seqexec.model.{ Observer, Operator}
import seqexec.model.dhs.ImageFileId
import seqexec.server.{InstrumentSystem, SeqActionF, OcsBuildInfo, sgoEq}
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

  val p: SeqActionF[F, Option[Double]] = for {
    xoffOpt <- tcsReader.getXOffset
    yoffOpt <- tcsReader.getYOffset
    iaaOpt <- tcsReader.getInstrumentAA
  } yield for {
    xoff <- xoffOpt
    yoff <- yoffOpt
    iaa <- iaaOpt
  } yield -xoff * Math.cos(Math.toRadians(iaa)) + yoff * Math.sin(Math.toRadians(iaa))

  val q: SeqActionF[F, Option[Double]] = for {
    xoffOpt <- tcsReader.getXOffset
    yoffOpt <- tcsReader.getYOffset
    iaaOpt  <- tcsReader.getInstrumentAA
  } yield for {
    xoff <- xoffOpt
    yoff <- yoffOpt
    iaa  <- iaaOpt
  } yield -xoff * Math.sin(Math.toRadians(iaa)) - yoff * Math.cos(Math.toRadians(iaa))

  val raoff: SeqActionF[F, Option[Double]] = for {
    poffOpt <- p
    qoffOpt <- q
    ipaOpt  <- tcsReader.getInstrumentPA
  } yield for {
    poff <- poffOpt
    qoff <- qoffOpt
    ipa  <- ipaOpt
  } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))

  val decoff: SeqActionF[F, Option[Double]] = for {
    poffOpt <- p
    qoffOpt <- q
    ipaOpt  <- tcsReader.getInstrumentPA
  } yield for {
    poff <- poffOpt
    qoff <- qoffOpt
    ipa  <- ipaOpt
  } yield poff * Math.cos(Math.toRadians(ipa)) + qoff * Math.sin(Math.toRadians(ipa))


  val obsObject: SeqActionF[F, Option[String]] = for {
    obsType   <- obsReader.getObsType
    obsObject <- obsReader.getObsObject
    tcsObject <- tcsReader.getSourceATarget.getObjectName
  } yield if (obsType === "OBJECT" && obsObject =!= "Twilight" && obsObject =!= "Domeflat") tcsObject
          else Some(obsObject)

  private def decodeGuide(v: StandardGuideOptions.Value): String = v match {
    case StandardGuideOptions.Value.park   => "parked"
    case StandardGuideOptions.Value.guide  => "guiding"
    case StandardGuideOptions.Value.freeze => "frozen"
  }

  private def optTcsKeyword[B](s: TcsController.Subsystem)(v: SeqActionF[F, B])(implicit d: DefaultHeaderValue[B]) : SeqActionF[F, B] =
    if(tcsSubsystems.contains(s)) v
    else SeqActionF(d.default)

  private def mountTcsKeyword[B](v: SeqActionF[F, B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.Mount)(v)(d)

  private def m2TcsKeyword[B](v: SeqActionF[F, B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.M2)(v)(d)

  private def sfTcsKeyword[B](v: SeqActionF[F, B])(implicit d: DefaultHeaderValue[B]) = optTcsKeyword[B](TcsController.Subsystem.AGUnit)(v)(d)

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
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getParallax.orDefault), KeywordName.PARALLAX),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getRadialVelocity.orDefault), KeywordName.RADVEL),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getEpoch.orDefault), KeywordName.EPOCH),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getEquinox.orDefault), KeywordName.EQUINOX),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingEquinox.orDefault), KeywordName.TRKEQUIN),
    buildString(stateReader.getOperatorName, KeywordName.SSA),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getRA.orDefault), KeywordName.RA),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getDec.orDefault), KeywordName.DEC),
    buildDouble(tcsReader.getElevation.orDefault, KeywordName.ELEVATIO),
    buildDouble(tcsReader.getAzimuth.orDefault, KeywordName.AZIMUTH),
    buildDouble(mountTcsKeyword(tcsReader.getCRPositionAngle.orDefault), KeywordName.CRPA),
    buildString(tcsReader.getHourAngle.orDefault, KeywordName.HA),
    buildString(tcsReader.getLocalTime.orDefault, KeywordName.LT),
    buildString(mountTcsKeyword(tcsReader.getTrackingFrame.orDefault), KeywordName.TRKFRAME),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingDec.orDefault), KeywordName.DECTRACK),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingEpoch.orDefault), KeywordName.TRKEPOCH),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingRA.orDefault), KeywordName.RATRACK),
    buildString(mountTcsKeyword(tcsReader.getSourceATarget.getFrame.orDefault), KeywordName.FRAME),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getProperMotionDec.orDefault), KeywordName.PMDEC),
    buildDouble(mountTcsKeyword(tcsReader.getSourceATarget.getProperMotionRA.orDefault), KeywordName.PMRA),
    {
      val x = tcsReader.getSourceATarget.getWavelength.map(_.map(_.length.toAngstroms))
      buildDouble(mountTcsKeyword(x.orDefault), KeywordName.WAVELENG)
    },
    buildString(stateReader.getRawImageQuality, KeywordName.RAWIQ),
    buildString(stateReader.getRawCloudCover, KeywordName.RAWCC),
    buildString(stateReader.getRawWaterVapor, KeywordName.RAWWV),
    buildString(stateReader.getRawBackgroundLight, KeywordName.RAWBG),
    buildString(obsReader.getPIReq, KeywordName.RAWPIREQ),
    buildString(obsReader.getGeminiQA, KeywordName.RAWGEMQA),
    buildString(tcsReader.getCarouselMode.orDefault, KeywordName.CGUIDMOD),
    buildString(tcsReader.getUT.orDefault, KeywordName.UT),
    buildString(tcsReader.getDate.orDefault, KeywordName.DATE),
    buildString(m2TcsKeyword(tcsReader.getM2Baffle.orDefault), KeywordName.M2BAFFLE),
    buildString(m2TcsKeyword(tcsReader.getM2CentralBaffle.orDefault), KeywordName.M2CENBAF),
    buildString(tcsReader.getST.orDefault, KeywordName.ST),
    buildDouble(mountTcsKeyword(tcsReader.getXOffset.orDefault), KeywordName.XOFFSET),
    buildDouble(mountTcsKeyword(tcsReader.getYOffset.orDefault), KeywordName.YOFFSET),
    buildDouble(mountTcsKeyword(p.orDefault), KeywordName.POFFSET),
    buildDouble(mountTcsKeyword(q.orDefault), KeywordName.QOFFSET),
    buildDouble(mountTcsKeyword(raoff.orDefault), KeywordName.RAOFFSET),
    buildDouble(mountTcsKeyword(decoff.orDefault), KeywordName.DECOFFSE),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingRAOffset.orDefault), KeywordName.RATRGOFF),
    buildDouble(mountTcsKeyword(tcsReader.getTrackingDecOffset.orDefault), KeywordName.DECTRGOF),
    buildDouble(mountTcsKeyword(tcsReader.getInstrumentPA.orDefault), KeywordName.PA),
    buildDouble(mountTcsKeyword(tcsReader.getInstrumentAA.orDefault), KeywordName.IAA),
    buildDouble(sfTcsKeyword(tcsReader.getSFRotation.orDefault), KeywordName.SFRT2),
    buildDouble(sfTcsKeyword(tcsReader.getSFTilt.orDefault), KeywordName.SFTILT),
    buildDouble(sfTcsKeyword(tcsReader.getSFLinear.orDefault), KeywordName.SFLINEAR),
    buildString(mountTcsKeyword(tcsReader.getAOFoldName.orDefault), KeywordName.AOFOLD),
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
    if (!requested.isEmpty) sendKeywords[F](id, inst, requested)
    else Applicative[F].unit
  }

  override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
    def guiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String, target: TargetKeywordsReader[F],
                       extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = guideWith.flatMap { g =>
      val keywords: List[KeywordBag => F[KeywordBag]] = List(
        KeywordName.fromTag(baseName + "ARA").map(buildDouble(target.getRA.orDefault, _)),
        KeywordName.fromTag(baseName + "ADEC").map(buildDouble(target.getDec.orDefault, _)),
        KeywordName.fromTag(baseName + "ARV").map(buildDouble(target.getRadialVelocity.orDefault, _)), {
          val x = target.getWavelength.map(_.map(_.length.toAngstroms))
          KeywordName.fromTag(baseName + "AWAVEL").map(buildDouble(x.orDefault, _))
        },
        KeywordName.fromTag(baseName + "AEPOCH").map(buildDouble(target.getEpoch.orDefault, _)),
        KeywordName.fromTag(baseName + "AEQUIN").map(buildDouble(target.getEquinox.orDefault, _)),
        KeywordName.fromTag(baseName + "AFRAME").map(buildString(target.getFrame.orDefault, _)),
        KeywordName.fromTag(baseName + "AOBJEC").map(buildString(target.getObjectName.orDefault, _)),
        KeywordName.fromTag(baseName + "APMDEC").map(buildDouble(target.getProperMotionDec.orDefault, _)),
        KeywordName.fromTag(baseName + "APMRA").map(buildDouble(target.getProperMotionRA.orDefault, _)),
        KeywordName.fromTag(baseName + "APARAL").map(buildDouble(target.getParallax.orDefault, _))
      ).collect { case Some(k) => k }

      if (g === StandardGuideOptions.Value.guide) sendKeywords[F](id, inst, keywords ++ extras)
      else Applicative[F].unit
    }

    def standardGuiderKeywords(guideWith: F[StandardGuideOptions.Value], baseName: String,
                               target: TargetKeywordsReader[F], extras: List[KeywordBag => F[KeywordBag]]): F[Unit] = {
      val ext = KeywordName.fromTag(baseName + "FOCUS").map(buildDouble(tcsReader.getM2UserFocusOffset.orDefault, _)).toList ++ extras
      guiderKeywords(guideWith, baseName, target, ext)
    }

    val oiwfsKeywords = guiderKeywords(obsReader.getOiwfsGuide.liftF, "OI", tcsReader.getOiwfsTarget,
      List(buildDouble(tcsReader.getOiwfsFreq.orDefault, KeywordName.OIFREQ)))

    val pwfs1Keywords = standardGuiderKeywords(obsReader.getPwfs1Guide.liftF, "P1", tcsReader.getPwfs1Target,
      List(buildDouble(tcsReader.getPwfs1Freq.orDefault, KeywordName.P1FREQ)))

    val pwfs2Keywords = standardGuiderKeywords(obsReader.getPwfs2Guide.liftF, "P2", tcsReader.getPwfs2Target,
      List(buildDouble(tcsReader.getPwfs2Freq.orDefault, KeywordName.P2FREQ)))

    val aowfsKeywords = standardGuiderKeywords(obsReader.getAowfsGuide.liftF, "AO", tcsReader.getAowfsTarget, Nil)

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
      buildDouble(tcsReader.getAirMass.orDefault, KeywordName.AIRMASS),
      buildDouble(tcsReader.getStartAirMass.orDefault, KeywordName.AMSTART),
      buildDouble(tcsReader.getEndAirMass.orDefault, KeywordName.AMEND),
      buildBoolean(obsReader.getHeaderPrivacy, KeywordName.PROP_MD),
      buildString(obsReader.getProprietaryMonths, KeywordName.RELEASE)
    ))
}
