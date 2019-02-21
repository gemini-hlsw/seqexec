// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.{EitherT, Nested, OneAnd, OptionT}
import cats.effect.IO
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.core.Wavelength
import seqexec.server.EpicsCodex.{DecodeEpicsValue, decode}
import seqexec.server.tcs.TcsController.FollowOption.{FollowOff, FollowOn}
import seqexec.server.tcs.TcsController.MountGuideOption.{MountGuideOff, MountGuideOn}
import seqexec.server.{SeqAction, SeqexecFailure, TrySeq}
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpics.{AoFold, EpicsTcsConfig}
import squants.{Angle, Length}
import squants.space.{Angstroms, Degrees, Millimeters}

object TcsConfigRetriever {

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Int, MountGuideOption] = DecodeEpicsValue((d: Int)
  => if (d === 0) MountGuideOff else MountGuideOn)

  implicit private val decodeM1GuideSource: DecodeEpicsValue[String, M1Source] = DecodeEpicsValue((s: String)
  => s.trim match {
      case "PWFS1" => M1Source.PWFS1
      case "PWFS2" => M1Source.PWFS2
      case "OIWFS" => M1Source.OIWFS
      case "GAOS"  => M1Source.GAOS
      case _       => M1Source.PWFS1
    })

  private def decodeM1Guide(r: BinaryOnOff, s: M1Source): M1GuideConfig =
    if (r === BinaryOnOff.Off) M1GuideOff
    else M1GuideOn(s)

  private def decodeGuideSourceOption(s: String): Boolean = s.trim =!= "OFF"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") ComaOption.ComaOff else ComaOption.ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s === BinaryOnOff.Off) M2GuideOff
    else M2GuideOn(u, v)

  implicit private val decodeAoFold: DecodeEpicsValue[String, AoFold] = DecodeEpicsValue((s: String) =>
    if(s.trim === "IN") AoFold.In
    else AoFold.Out
  )

  private def getGuideConfig: SeqAction[TelescopeGuideConfig] = EitherT ( {
    for {
      mountGuide <-  OptionT(TcsEpics.instance.absorbTipTilt).map(decode[Int, MountGuideOption])
      m1Source   <-  OptionT(TcsEpics.instance.m1GuideSource).map(decode[String, M1Source])
      m1Guide    <-  OptionT(TcsEpics.instance.m1Guide).map(decodeM1Guide(_, m1Source))
      m2p1Guide  <-  OptionT(TcsEpics.instance.m2p1Guide).map(decodeGuideSourceOption)
      m2p2Guide  <-  OptionT(TcsEpics.instance.m2p2Guide).map(decodeGuideSourceOption)
      m2oiGuide  <-  OptionT(TcsEpics.instance.m2oiGuide).map(decodeGuideSourceOption)
      m2aoGuide  <-  OptionT(TcsEpics.instance.m2aoGuide).map(decodeGuideSourceOption)
      m2Coma     <-  OptionT(TcsEpics.instance.comaCorrect).map(decode[String, ComaOption])
      m2Guide    <- OptionT(Nested(TcsEpics.instance.m2GuideState).map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set.empty[TipTiltSource])((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s))).value)
    } yield TrySeq(TelescopeGuideConfig(mountGuide, m1Guide, m2Guide))
  }.value.map(_.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")))))

  private def getAoFold: SeqAction[AoFold] =
    getStatusVal(TcsEpics.instance.aoFoldPosition.map(_.map(decode[String, AoFold])), "AO Fold")

  private def decodeNodChopOption(s: String): Boolean = s.trim === "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig[IO]): IO[Option[NodChopTrackingConfig]] = (
    for {
      aa <-  OptionT(g.nodachopa).map(decodeNodChopOption)
      ab <-  OptionT(g.nodachopb).map(decodeNodChopOption)
      ac <-  OptionT(g.nodachopc).map(decodeNodChopOption)
      ba <-  OptionT(g.nodbchopa).map(decodeNodChopOption)
      bb <-  OptionT(g.nodbchopb).map(decodeNodChopOption)
      bc <-  OptionT(g.nodbchopc).map(decodeNodChopOption)
      ca <-  OptionT(g.nodcchopa).map(decodeNodChopOption)
      cb <-  OptionT(g.nodcchopb).map(decodeNodChopOption)
      cc <-  OptionT(g.nodcchopc).map(decodeNodChopOption)

      // This last product is slightly tricky.
      o  <- OptionT(IO{
        if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).contains(true)) {
              if (List(aa, bb, cc).forall(_ === true) && List(ab, ac, ba, bc, ca, cb).forall(_ === false)) {
                Some(NodChopTrackingConfig.Normal)
              } else {
                List(
                  (aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)), (ac, NodChop(Beam.A, Beam.C)),
                  (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)), (bc, NodChop(Beam.B, Beam.C)),
                  (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)), (cc, NodChop(Beam.C, Beam.C))
                ) collect {
                  case (true, a) => a
                } match {
                  case h :: t => Some(NodChopTrackingConfig.Special(OneAnd(h, t)))
                  case Nil    => None // the list is empty
                }
              }
            } else Some(NodChopTrackingConfig.None)
      })
    } yield o
  ).value

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (_, NodChopTrackingConfig.None)              => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.Normal)     => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    case (FollowOn, v: NodChopTrackingConfig.Special) => ProbeTrackingConfig.On(v)
    case _                                            => ProbeTrackingConfig.Off
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") FollowOff else FollowOn)

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] =
    DecodeEpicsValue((s: BinaryYesNo) => if (s === BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)

  private def getPwfs1: SeqAction[GuiderConfig] = for {
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig), "PWFS1 tracking configuration")
    fol <- getStatusVal(TcsEpics.instance.p1FollowS.map(_.map(decode[String, FollowOption])), "PWFS1 follow")
    wfs <- getStatusVal(TcsEpics.instance.pwfs1On.map(_.map(decode[BinaryYesNo, GuiderSensorOption])), "PWFS1 detector")
  } yield GuiderConfig(calcProbeTrackingConfig(fol, trk), wfs)

  private def getPwfs2: SeqAction[GuiderConfig] = for {
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig), "PWFS2 tracking configuration")
    fol <- getStatusVal(TcsEpics.instance.p2FollowS.map(_.map(decode[String, FollowOption])), "PWFS2 follow")
    wfs <- getStatusVal(TcsEpics.instance.pwfs2On.map(_.map(decode[BinaryYesNo, GuiderSensorOption])), "PWFS2 detector")
  } yield GuiderConfig(calcProbeTrackingConfig(fol, trk), wfs)

  private def getOiwfs: SeqAction[GuiderConfig] = for {
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig), "OIWFS tracking configuration")
    fol <- getStatusVal(TcsEpics.instance.oiFollowS.map(_.map(decode[String, FollowOption])), "OIWFS follow")
    wfs <- getStatusVal(TcsEpics.instance.oiwfsOn.map(_.map(decode[BinaryYesNo, GuiderSensorOption])), "OIWFS detector")
  } yield GuiderConfig(calcProbeTrackingConfig(fol, trk), wfs)

  import ScienceFoldPositionCodex._

  private def getScienceFoldPosition: SeqAction[ScienceFoldPosition] =
    for {
      sfPos <- getStatusVal(TcsEpics.instance.sfName.map(_.flatMap(decode[String, Option[ScienceFoldPosition]])),"SF position")
      sfParked <- getStatusVal(TcsEpics.instance.sfParked.map(_.map(_ =!= 0)), "SF park")
    } yield if (sfParked) ScienceFoldPosition.Parked
            else sfPos

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: SeqAction[HrwfsPickupPosition] =
    for {
      hwPos <- getStatusVal(TcsEpics.instance.agHwName.map(_.map(decode[String, HrwfsPickupPosition])), "Pickup position")
      hwParked <- getStatusVal(TcsEpics.instance.agHwParked.map(_.map(_ =!= 0)), "Pickup park")
    } yield if (hwParked) HrwfsPickupPosition.Parked
      else hwPos

  private def getStatusVal[A](get: IO[Option[A]], name: String): SeqAction[A] = EitherT(get.map(
    _.fold(TrySeq.fail[A](SeqexecFailure.Unexpected(s"Unable to read $name from TCS.")))(TrySeq(_))
  ))

  private def getIAA: SeqAction[Angle] = getStatusVal(TcsEpics.instance.instrAA.map(_.map(Degrees(_))), "IAA")

  private def getOffsetX: SeqAction[Length] = getStatusVal(TcsEpics.instance.xoffsetPoA1.map(_.map(Millimeters(_))),
    "X offset")

  private def getOffsetY: SeqAction[Length] = getStatusVal(TcsEpics.instance.yoffsetPoA1.map(_.map(Millimeters(_))),
    "Y offset")

  private def getWavelength: SeqAction[Wavelength] =
    getStatusVal(TcsEpics.instance.sourceAWavelength.map(_.map(v => Wavelength(Angstroms(v)))), "central wavelength")

  def retrieveConfiguration: SeqAction[EpicsTcsConfig] =
    for {
      iaa  <- getIAA
      offX <- getOffsetX
      offY <- getOffsetY
      wl   <- getWavelength
      p1   <- getPwfs1
      p2   <- getPwfs2
      oi   <- getOiwfs
      tgc  <- getGuideConfig
      aof  <- getAoFold
      sf   <- getScienceFoldPosition
      hr   <- getHrwfsPickupPosition
    } yield EpicsTcsConfig(iaa, offX, offY, wl, p1, p2, oi, tgc, aof, sf, hr)

}
