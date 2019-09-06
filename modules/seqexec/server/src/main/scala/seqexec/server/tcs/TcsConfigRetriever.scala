// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Eq
import cats.data.OneAnd
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
import seqexec.server.SeqexecFailure.NullEpicsError
import seqexec.server.gems.Gems.{Cwfs1DetectorState, Cwfs2DetectorState, Cwfs3DetectorState, DetectorStateOps, GemsWfsState, Odgw1DetectorState, Odgw2DetectorState, Odgw3DetectorState, Odgw4DetectorState}
import seqexec.server.gems.Gems.Cwfs1DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Cwfs2DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Cwfs3DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Odgw1DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Odgw2DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Odgw3DetectorState.{Off => _, On => _, _}
import seqexec.server.gems.Gems.Odgw4DetectorState.{Off => _, On => _, _}
import seqexec.server.tcs.GemsSource.{Cwfs1, Cwfs2, Cwfs3, Odgw1, Odgw2, Odgw3, Odgw4}
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsControllerEpicsCommon.{AoFold, InstrumentPorts, InvalidPort, ScienceFold}
import seqexec.server.tcs.TcsEpics.VirtualGemsTelescope
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
      mountGuide <-  TcsEpics.instance.absorbTipTilt.map(decode[Int, MountGuideOption])
      m1Source   <-  TcsEpics.instance.m1GuideSource.map(decode[String, M1Source])
      m1Guide    <-  TcsEpics.instance.m1Guide.map(decodeM1Guide(_, m1Source))
      m2p1Guide  <-  TcsEpics.instance.m2p1Guide.map(decodeGuideSourceOption)
      m2p2Guide  <-  TcsEpics.instance.m2p2Guide.map(decodeGuideSourceOption)
      m2oiGuide  <-  TcsEpics.instance.m2oiGuide.map(decodeGuideSourceOption)
      m2aoGuide  <-  TcsEpics.instance.m2aoGuide.map(decodeGuideSourceOption)
      m2Coma     <-  TcsEpics.instance.comaCorrect.map(decode[String, ComaOption])
      m2Guide    <-  TcsEpics.instance.m2GuideState.map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set.empty[TipTiltSource])((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s)))
    } yield TelescopeGuideConfig(mountGuide, m1Guide, m2Guide)
  }.adaptError{ case e => SeqexecFailure.Unexpected(s"Unable to read guide configuration from TCS: $e")}

  private def getAoFold: IO[AoFold] = TcsEpics.instance.aoFoldPosition.map(decode[String, AoFold])

  private def decodeNodChopOption(s: String): Boolean = s.trim === "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig[IO]): IO[NodChopTrackingConfig] =
    for {
      aa <- g.nodachopa.map(decodeNodChopOption)
      ab <- g.nodachopb.map(decodeNodChopOption)
      ac <- g.nodachopc.map(decodeNodChopOption)
      ba <- g.nodbchopa.map(decodeNodChopOption)
      bb <- g.nodbchopb.map(decodeNodChopOption)
      bc <- g.nodbchopc.map(decodeNodChopOption)
      ca <- g.nodcchopa.map(decodeNodChopOption)
      cb <- g.nodcchopb.map(decodeNodChopOption)
      cc <- g.nodcchopc.map(decodeNodChopOption)
    } yield
      if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).contains(true)) {
        if (List(aa, bb).forall(_ === true) && List(ab, ac, ba, bc, ca, cb, cc).forall(_ === false)) {
          NodChopTrackingConfig.Normal
        } else {
          List(
            (aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)), (ac, NodChop(Beam.A, Beam.C)),
            (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)), (bc, NodChop(Beam.B, Beam.C)),
            (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)), (cc, NodChop(Beam.C, Beam.C))
          ) collect {
            case (true, a) => a
          } match {
            case h :: t => NodChopTrackingConfig.Special(OneAnd(h, t))
            case Nil    => NodChopTrackingConfig.AllOff
          }
        }
      } else NodChopTrackingConfig.AllOff

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
    prk <- TcsEpics.instance.p1Parked
    trk <- getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig)
    fol <- TcsEpics.instance.p1FollowS.map(decode[String, FollowOption])
    wfs <- TcsEpics.instance.pwfs1On.map(decode[BinaryYesNo, GuiderSensorOption])
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  private def getPwfs2: IO[GuiderConfig] = for {
    prk   <- TcsEpics.instance.p2Parked
    trk   <- getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig)
    fol   <- TcsEpics.instance.p2FollowS.map(decode[String, FollowOption])
    wfs   <- TcsEpics.instance.pwfs2On.map(decode[BinaryYesNo, GuiderSensorOption])
    useAo <- getUseAo
  } yield if(useAo) GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, NodChopTrackingConfig.AllOff)), wfs)
          else GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  private def getUseAo: IO[Boolean] = TcsEpics.instance.useAo.map(_ === BinaryYesNo.Yes)

  private def getAowfs(getAoFollow: IO[Boolean]): IO[ProbeTrackingConfig] = for {
    aoFol <- getAoFollow.map(if(_) FollowOn else FollowOff)
    trk   <- getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig)
    useAo <- getUseAo
  } yield if(useAo) calcProbeTrackingConfig(aoFol, trk)
          else calcProbeTrackingConfig(aoFol, NodChopTrackingConfig.AllOff)

  private def getOiwfs: IO[GuiderConfig] = for {
    prk <- TcsEpics.instance.oiParked
    trk <- getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig)
    fol <- TcsEpics.instance.oiFollowS.map(decode[String, FollowOption])
    wfs <- TcsEpics.instance.oiwfsOn.map(decode[BinaryYesNo, GuiderSensorOption])
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  import ScienceFoldPositionCodex._

  private def getScienceFoldPosition: IO[Option[ScienceFold]] = for {
    sfPos    <- TcsEpics.instance.sfName
    sfParked <- TcsEpics.instance.sfParked.map(_ =!= 0)
  } yield if (sfParked) ScienceFold.Parked.some
          else decode[String, Option[ScienceFold]](sfPos)

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: IO[HrwfsPickupPosition] = for {
    hwPos    <- TcsEpics.instance.agHwName.map(decode[String, HrwfsPickupPosition])
    hwParked <- TcsEpics.instance.agHwParked.map(_ =!= 0)
  } yield if (hwParked) HrwfsPickupPosition.Parked
          else hwPos

  private def getIAA: IO[Angle] = TcsEpics.instance.instrAA.map(Degrees(_))

  private def getOffsetX: IO[Length] = TcsEpics.instance.xoffsetPoA1.map(Millimeters(_))

  private def getOffsetY: IO[Length] = TcsEpics.instance.yoffsetPoA1.map(Millimeters(_))

  private def getWavelength: IO[Wavelength] = TcsEpics.instance.sourceAWavelength.map(v => Wavelength(Angstroms(v)))

  private def getGemsMap: IO[Map[GemsSource, VirtualGemsTelescope]] = for {
    v1 <- TcsEpics.instance.g1MapName
    v2 <- TcsEpics.instance.g2MapName
    v3 <- TcsEpics.instance.g3MapName
    v4 <- TcsEpics.instance.g4MapName
  } yield List(
    v1 -> VirtualGemsTelescope.G1,
    v2 -> VirtualGemsTelescope.G2,
    v3 -> VirtualGemsTelescope.G3,
    v4 -> VirtualGemsTelescope.G4
  ).mapFilter{ case (s, v) => s.map((_, v))}.toMap

  private def getCwfs[T: DetectorStateOps: Eq](getFollow: IO[Boolean])
                                             (g: VirtualGemsTelescope, active: IO[T])
  : IO[GuiderConfig] = for {
    trk <- getNodChopTrackingConfig(TcsEpics.instance.gemsGuideConfig(g))
    fol <- getFollow.map{if(_) FollowOption.FollowOn else FollowOption.FollowOff}
    wfs <- active.map{x => if(DetectorStateOps.isActive(x)) GuiderSensorOn else GuiderSensorOff}
  } yield GuiderConfig(calcProbeTrackingConfig(fol, trk), wfs)

  private val getCwfs1: (VirtualGemsTelescope, IO[Cwfs1DetectorState]) => IO[GuiderConfig] =
    getCwfs(TcsEpics.instance.cwfs1Follow)

  private val getCwfs2: (VirtualGemsTelescope, IO[Cwfs2DetectorState]) => IO[GuiderConfig] =
    getCwfs(TcsEpics.instance.cwfs1Follow)

  private val getCwfs3: (VirtualGemsTelescope, IO[Cwfs3DetectorState]) => IO[GuiderConfig] =
    getCwfs(TcsEpics.instance.cwfs1Follow)

  private def getOdgw[T: DetectorStateOps: Eq](getParked: IO[Boolean], getFollow: IO[Boolean])
                                          (g: VirtualGemsTelescope, active: IO[T])
  : IO[GuiderConfig] = for {
    prk <- getParked
    trk <- getNodChopTrackingConfig(TcsEpics.instance.gemsGuideConfig(g))
    fol <- getFollow.map{ if(_) FollowOption.FollowOn else FollowOption.FollowOff}
    wfs <- active.map{x => if(DetectorStateOps.isActive[T](x)) GuiderSensorOn else GuiderSensorOff}
  } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

  private val getOdgw1: (VirtualGemsTelescope, IO[Odgw1DetectorState]) => IO[GuiderConfig] =
    getOdgw(TcsEpics.instance.odgw1Parked, TcsEpics.instance.odgw1Follow)

  private val getOdgw2: (VirtualGemsTelescope, IO[Odgw2DetectorState]) => IO[GuiderConfig] =
    getOdgw(TcsEpics.instance.odgw2Parked, TcsEpics.instance.odgw2Follow)

  private val getOdgw3: (VirtualGemsTelescope, IO[Odgw3DetectorState]) => IO[GuiderConfig] =
    getOdgw(TcsEpics.instance.odgw3Parked, TcsEpics.instance.odgw3Follow)

  private val getOdgw4: (VirtualGemsTelescope, IO[Odgw4DetectorState]) => IO[GuiderConfig] =
    getOdgw(TcsEpics.instance.odgw4Parked, TcsEpics.instance.odgw4Follow)

  private def getInstrumentPorts: IO[InstrumentPorts] = for {
    f2    <- TcsEpics.instance.f2Port.recover{case NullEpicsError(_) => InvalidPort}
    ghost <- TcsEpics.instance.ghostPort.recover{case NullEpicsError(_) => InvalidPort}
    gmos  <- TcsEpics.instance.gmosPort.recover{case NullEpicsError(_) => InvalidPort}
    gnirs <- TcsEpics.instance.gnirsPort.recover{case NullEpicsError(_) => InvalidPort}
    gpi   <- TcsEpics.instance.gpiPort.recover{case NullEpicsError(_) => InvalidPort}
    gsaoi <- TcsEpics.instance.gsaoiPort.recover{case NullEpicsError(_) => InvalidPort}
    nifs  <- TcsEpics.instance.nifsPort.recover{case NullEpicsError(_) => InvalidPort}
    niri  <- TcsEpics.instance.niriPort.recover{case NullEpicsError(_) => InvalidPort}
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

  def retrieveConfigurationNorth(getAoFollow: IO[Boolean]): IO[TcsNorthControllerEpicsAo.EpicsTcsAoConfig] =
    for {
      base <- retrieveBaseConfiguration
      ao   <- getAowfs(getAoFollow)
    } yield TcsNorthControllerEpicsAo.EpicsTcsAoConfig(base, ao)

  private def retrieveGemsGuider(mapping: Map[GemsSource, VirtualGemsTelescope],
                                 gemsSource: GemsSource,
                                 getGuide: VirtualGemsTelescope => IO[GuiderConfig]): IO[GuiderConfig] =
    mapping.get(gemsSource).map(getGuide).getOrElse(IO(GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff)))

  def retrieveConfigurationSouth(gemsSt: GemsWfsState[IO]): IO[TcsSouthControllerEpicsAo.EpicsTcsAoConfig] =
    for {
      base    <- retrieveBaseConfiguration
      mapping <- getGemsMap
      cwfs1   <- retrieveGemsGuider(mapping, Cwfs1, getCwfs1(_, gemsSt.cwfs1))
      cwfs2   <- retrieveGemsGuider(mapping, Cwfs2, getCwfs2(_, gemsSt.cwfs2))
      cwfs3   <- retrieveGemsGuider(mapping, Cwfs3, getCwfs3(_, gemsSt.cwfs3))
      odgw1   <- retrieveGemsGuider(mapping, Odgw1, getOdgw1(_, gemsSt.odgw1))
      odgw2   <- retrieveGemsGuider(mapping, Odgw2, getOdgw2(_, gemsSt.odgw2))
      odgw3   <- retrieveGemsGuider(mapping, Odgw3, getOdgw3(_, gemsSt.odgw3))
      odgw4   <- retrieveGemsGuider(mapping, Odgw4, getOdgw4(_, gemsSt.odgw4))
    } yield TcsSouthControllerEpicsAo.EpicsTcsAoConfig(
      base,
      mapping,
      cwfs1,
      cwfs2,
      cwfs3,
      odgw1,
      odgw2,
      odgw3,
      odgw4
    )

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
