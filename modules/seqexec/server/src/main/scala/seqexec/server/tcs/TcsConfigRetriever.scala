// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.MonadError
import cats.data.{Nested, OneAnd, OptionT}
import cats.effect.IO
import cats.implicits._
import mouse.boolean._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.core.Wavelength
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.ComaOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.TipTiltSource
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.EpicsCodex.{DecodeEpicsValue, decode}
import seqexec.server.tcs.TcsController.FollowOption.{FollowOff, FollowOn}
import seqexec.server.SeqexecFailure
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpicsCommon.{AoFold, InstrumentPorts, InvalidPort, ScienceFold}
import shapeless.tag
import squants.{Angle, Length}
import squants.space.{Angstroms, Degrees, Millimeters}

object TcsConfigRetriever {

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Int, MountGuideOption] = DecodeEpicsValue((d: Int)
  => if (d === 0) MountGuideOption.MountGuideOff else MountGuideOption.MountGuideOn)

  implicit private val decodeM1GuideSource: DecodeEpicsValue[String, M1Source] = DecodeEpicsValue((s: String)
  => s.trim match {
      case "PWFS1" => M1Source.PWFS1
      case "PWFS2" => M1Source.PWFS2
      case "OIWFS" => M1Source.OIWFS
      case "GAOS"  => M1Source.GAOS
      case _       => M1Source.PWFS1
    })

  private def decodeM1Guide(r: BinaryOnOff, s: M1Source): M1GuideConfig =
    if (r === BinaryOnOff.Off) M1GuideConfig.M1GuideOff
    else M1GuideConfig.M1GuideOn(s)

  private def decodeGuideSourceOption(s: String): Boolean = s.trim =!= "OFF"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") ComaOption.ComaOff else ComaOption.ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s === BinaryOnOff.Off) M2GuideConfig.M2GuideOff
    else M2GuideConfig.M2GuideOn(u, v)

  implicit private val decodeAoFold: DecodeEpicsValue[String, AoFold] = DecodeEpicsValue((s: String) =>
    if(s.trim === "IN") AoFold.In
    else AoFold.Out
  )

  private def getGuideConfig: IO[TelescopeGuideConfig] = {
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
    } yield TelescopeGuideConfig(mountGuide, m1Guide, m2Guide)
  }.getOrElseF(IO.raiseError(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")))

  private def getAoFold: IO[AoFold] =
    getStatusVal(Nested(TcsEpics.instance.aoFoldPosition).map(decode[String, AoFold]).value, "AO Fold")

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
          if (List(aa, bb).forall(_ === true) && List(ab, ac, ba, bc, ca, cb, cc).forall(_ === false)) {
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
        } else Some(NodChopTrackingConfig.AllOff)
      })
    } yield o
  ).value

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (FollowOff, _)                            => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.AllOff)  => ProbeTrackingConfig.Frozen
    case (FollowOn, v:ActiveNodChopTracking)       => ProbeTrackingConfig.On(v)
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") FollowOff else FollowOn)

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] =
    DecodeEpicsValue((s: BinaryYesNo) => if (s === BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)

  private def getPwfs1: IO[GuiderConfig] = for {
    prk <- getStatusVal(TcsEpics.instance.p1Parked, "PWFS1 parked state")
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig), "PWFS1 tracking configuration")
    fol <- getStatusVal(Nested(TcsEpics.instance.p1FollowS).map(decode[String, FollowOption]).value, "PWFS1 follow state")
    wfs <- getStatusVal(Nested(TcsEpics.instance.pwfs1On).map(decode[BinaryYesNo, GuiderSensorOption]).value, "PWFS1 detector")
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  private def getPwfs2: IO[GuiderConfig] = for {
    prk   <- getStatusVal(TcsEpics.instance.p2Parked, "PWFS2 parked state")
    trk   <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig), "PWFS2 tracking configuration")
    fol   <- getStatusVal(Nested(TcsEpics.instance.p2FollowS).map(decode[String, FollowOption]).value, "PWFS2 follow state")
    wfs   <- getStatusVal(Nested(TcsEpics.instance.pwfs2On).map(decode[BinaryYesNo, GuiderSensorOption]).value, "PWFS2 detector")
    useAo <- getUseAo
  } yield if(useAo) GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, NodChopTrackingConfig.AllOff)), wfs)
          else GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  private def getUseAo: IO[Boolean] = getStatusVal(TcsEpics.instance.useAo, "use AO flag").map(_ === BinaryYesNo.Yes)

  private def getAowfs(getAoFollow: IO[Option[Boolean]]): IO[ProbeTrackingConfig] = for {
    aoFol <- getStatusVal(getAoFollow, "AO follow state").map(_.fold(FollowOn, FollowOff))
    trk   <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig), "AOWFS tracking configuration")
    useAo <- getUseAo
  } yield if(useAo) calcProbeTrackingConfig(aoFol, trk)
          else calcProbeTrackingConfig(aoFol, NodChopTrackingConfig.AllOff)

  private def getOiwfs: IO[GuiderConfig] = for {
    prk <- getStatusVal(TcsEpics.instance.oiParked, "OIWFS parked state")
    trk <- getStatusVal(getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig), "OIWFS tracking configuration")
    fol <- getStatusVal(Nested(TcsEpics.instance.oiFollowS).map(decode[String, FollowOption]).value, "OIWFS follow state")
    wfs <- getStatusVal(Nested(TcsEpics.instance.oiwfsOn).map(decode[BinaryYesNo, GuiderSensorOption]).value, "OIWFS detector")
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  import ScienceFoldPositionCodex._

  private def getScienceFoldPosition: IO[Option[ScienceFold]] = for {
    sfPos    <- getStatusVal(TcsEpics.instance.sfName,"SF position")
    sfParked <- getStatusVal(Nested(TcsEpics.instance.sfParked).map(_ =!= 0).value, "SF park")
  } yield if (sfParked) ScienceFold.Parked.some
          else decode[String, Option[ScienceFold]](sfPos)

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: IO[HrwfsPickupPosition] = for {
    hwPos    <- getStatusVal(Nested(TcsEpics.instance.agHwName).map(decode[String, HrwfsPickupPosition]).value,
      "Pickup position")
    hwParked <- getStatusVal(Nested(TcsEpics.instance.agHwParked).map(_ =!= 0).value, "Pickup park")
  } yield if (hwParked) HrwfsPickupPosition.Parked
          else hwPos

  private def getStatusVal[F[_]: MonadError[?[_], Throwable], A](get: F[Option[A]], name: String): F[A] =
    OptionT(get).getOrElseF(SeqexecFailure.Unexpected(s"Unable to read $name from TCS.").raiseError[F, A])

  private def getIAA: IO[Angle] = getStatusVal(Nested(TcsEpics.instance.instrAA).map(Degrees(_)).value, "IAA")

  private def getOffsetX: IO[Length] = getStatusVal(Nested(TcsEpics.instance.xoffsetPoA1).map(Millimeters(_)).value,
    "X offset")

  private def getOffsetY: IO[Length] = getStatusVal(Nested(TcsEpics.instance.yoffsetPoA1).map(Millimeters(_)).value,
    "Y offset")

  private def getWavelength: IO[Wavelength] =
    getStatusVal(Nested(TcsEpics.instance.sourceAWavelength).map(v => Wavelength(Angstroms(v))).value,
      "central wavelength")

  private def getInstrumentPorts: IO[InstrumentPorts] = for {
    f2    <- TcsEpics.instance.f2Port.map(_.getOrElse(InvalidPort))
    ghost <- TcsEpics.instance.ghostPort.map(_.getOrElse(InvalidPort))
    gmos  <- TcsEpics.instance.gmosPort.map(_.getOrElse(InvalidPort))
    gnirs <- TcsEpics.instance.gnirsPort.map(_.getOrElse(InvalidPort))
    gpi   <- TcsEpics.instance.gpiPort.map(_.getOrElse(InvalidPort))
    gsaoi <- TcsEpics.instance.gsaoiPort.map(_.getOrElse(InvalidPort))
    nifs  <- TcsEpics.instance.nifsPort.map(_.getOrElse(InvalidPort))
    niri  <- TcsEpics.instance.niriPort.map(_.getOrElse(InvalidPort))
  } yield InstrumentPorts(
    f2,
    ghost,
    gmos,
    gnirs,
    gpi,
    gsaoi,
    nifs,
    niri
  )

  def retrieveConfigurationNorth(getAoFollow: IO[Option[Boolean]]): IO[TcsNorthControllerEpicsAo.EpicsTcsAoConfig] =
    for {
      base <- retrieveBaseConfiguration
      ao   <- getAowfs(getAoFollow)
    } yield TcsNorthControllerEpicsAo.EpicsTcsAoConfig(base, ao)

  def retrieveBaseConfiguration: IO[TcsControllerEpicsCommon.BaseEpicsTcsConfig] =
    for {
      iaa   <- getIAA
      offX  <- getOffsetX
      offY  <- getOffsetY
      wl    <- getWavelength
      p1    <- getPwfs1
      p2    <- getPwfs2
      oi    <- getOiwfs
      tgc   <- getGuideConfig
      aof   <- getAoFold
      useAo <- getUseAo
      sf    <- getScienceFoldPosition
      hr    <- getHrwfsPickupPosition
      ports <- getInstrumentPorts
    } yield TcsControllerEpicsCommon.BaseEpicsTcsConfig(
      iaa,
      FocalPlaneOffset(tag[OffsetX](offX), tag[OffsetY](offY)),
      wl,
      p1,
      p2,
      oi,
      tgc,
      aof,
      useAo,
      sf,
      hr,
      ports
    )

}
