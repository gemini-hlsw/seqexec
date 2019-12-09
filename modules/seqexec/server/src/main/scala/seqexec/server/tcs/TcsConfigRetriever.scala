// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data.OneAnd
import cats.implicits._
import mouse.boolean._
import edu.gemini.seqexec.server.tcs.BinaryYesNo
import edu.gemini.spModel.core.Wavelength
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.ComaOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.TipTiltSource
import seqexec.model.TelescopeGuideConfig
import seqexec.server.EpicsCodex.decode
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
import seqexec.server.tcs.TcsEpics.VirtualGemsTelescope
import shapeless.tag
import squants.{Angle, Length}
import squants.space.{Angstroms, Degrees, Millimeters}

sealed trait TcsConfigRetriever[F[_]] {
  def retrieveBaseConfiguration: F[BaseEpicsTcsConfig]

  def retrieveConfigurationNorth(getAoFollow: F[Boolean]): F[TcsNorthControllerEpicsAo.EpicsTcsAoConfig]

  def retrieveConfigurationSouth(gemsSt: GemsWfsState[F]): F[TcsSouthControllerEpicsAo.EpicsTcsAoConfig]
}

object TcsConfigRetriever {
  private class TcsConfigRetrieverImpl[F[_]: MonadError[?[_], Throwable]](
    epicsSys: TcsEpics[F]) extends TcsConfigRetriever[F]
                           with    TcsConfigDecoders
                           with    ScienceFoldPositionCodex {

    private def getGuideConfig: F[TelescopeGuideConfig] = {
      for {
        mountGuide <- epicsSys.absorbTipTilt.map(decode[Int, MountGuideOption])
        m1Source   <- epicsSys.m1GuideSource.map(decode[String, M1Source])
        m1Guide    <- epicsSys.m1Guide.map(decodeM1Guide(_, m1Source))
        m2p1Guide  <- epicsSys.m2p1Guide.map(decodeGuideSourceOption)
        m2p2Guide  <- epicsSys.m2p2Guide.map(decodeGuideSourceOption)
        m2oiGuide  <- epicsSys.m2oiGuide.map(decodeGuideSourceOption)
        m2aoGuide  <- epicsSys.m2aoGuide.map(decodeGuideSourceOption)
        m2Coma     <- epicsSys.comaCorrect.map(decode[String, ComaOption])
        m2Guide    <- epicsSys.m2GuideState.map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
          (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
          (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set.empty[TipTiltSource])((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s)))
      } yield TelescopeGuideConfig(mountGuide, m1Guide, m2Guide)
    }.adaptError{ case e => SeqexecFailure.Unexpected(s"Unable to read guide configuration from TCS: $e")}

    private def getAoFold: F[AoFold] = epicsSys.aoFoldPosition.map(decode[String, AoFold])

    private def decodeNodChopOption(s: Int): Boolean = s =!= 0

    private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig[F]): F[NodChopTrackingConfig] =
      for {
        aa <- g.nodachopa.map(decodeNodChopOption)
        ab <- g.nodachopb.map(decodeNodChopOption)
        ba <- g.nodbchopa.map(decodeNodChopOption)
        bb <- g.nodbchopb.map(decodeNodChopOption)
      } yield
        if (List(aa, ab, ba, bb).contains(true)) {
          if (List(aa, bb).forall(_ === true) && List(ab, ba).forall(_ === false)) {
            NodChopTrackingConfig.Normal
          } else {
            List(
              (aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)),
              (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B))
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

    private def getPwfs1: F[GuiderConfig] = for {
      prk <- epicsSys.p1Parked
      trk <- getNodChopTrackingConfig(epicsSys.pwfs1ProbeGuideConfig)
      fol <- epicsSys.p1FollowS.map(decode[String, FollowOption])
      wfs <- epicsSys.pwfs1On.map(decode[BinaryYesNo, GuiderSensorOption])
    } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

    private def getPwfs2: F[GuiderConfig] = for {
      prk   <- epicsSys.p2Parked
      trk   <- getNodChopTrackingConfig(epicsSys.pwfs2ProbeGuideConfig)
      fol   <- epicsSys.p2FollowS.map(decode[String, FollowOption])
      wfs   <- epicsSys.pwfs2On.map(decode[BinaryYesNo, GuiderSensorOption])
      useAo <- getUseAo
    } yield if(useAo) GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, NodChopTrackingConfig.AllOff)), wfs)
            else GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

    private def getUseAo: F[Boolean] = epicsSys.useAo.map(_ === BinaryYesNo.Yes)

    private def getAowfs(getAoFollow: F[Boolean]): F[ProbeTrackingConfig] = for {
      aoFol <- getAoFollow.map(if(_) FollowOn else FollowOff)
      trk   <- getNodChopTrackingConfig(epicsSys.pwfs2ProbeGuideConfig)
      useAo <- getUseAo
    } yield if(useAo) calcProbeTrackingConfig(aoFol, trk)
            else calcProbeTrackingConfig(aoFol, NodChopTrackingConfig.AllOff)

    private def getOiwfs: F[GuiderConfig] = for {
      prk <- epicsSys.oiParked
      trk <- getNodChopTrackingConfig(epicsSys.oiwfsProbeGuideConfig)
      fol <- epicsSys.oiFollowS.map(decode[String, FollowOption])
      wfs <- epicsSys.oiwfsOn.map(decode[BinaryYesNo, GuiderSensorOption])
    } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

    private def getScienceFoldPosition: F[Option[ScienceFold]] = for {
      sfPos    <- epicsSys.sfName
      sfParked <- epicsSys.sfParked.map(_ =!= 0)
    } yield if (sfParked) ScienceFold.Parked.some
            else decode[String, Option[ScienceFold]](sfPos)

    private def getHrwfsPickupPosition: F[HrwfsPickupPosition] = for {
      hwPos    <- epicsSys.agHwName.map(decode[String, HrwfsPickupPosition])
      hwParked <- epicsSys.agHwParked.map(_ =!= 0)
    } yield if (hwParked) HrwfsPickupPosition.Parked
            else hwPos

    private def getIAA: F[Angle] = epicsSys.instrAA.map(Degrees(_))

    private def getOffsetX: F[Length] = epicsSys.xoffsetPoA1.map(Millimeters(_))

    private def getOffsetY: F[Length] = epicsSys.yoffsetPoA1.map(Millimeters(_))

    private def getWavelength: F[Wavelength] = epicsSys.sourceAWavelength.map(v => Wavelength(Angstroms(v)))

    private def getGemsMap: F[Map[GemsSource, VirtualGemsTelescope]] = for {
      v1 <- epicsSys.g1MapName
      v2 <- epicsSys.g2MapName
      v3 <- epicsSys.g3MapName
      v4 <- epicsSys.g4MapName
    } yield List(
      v1 -> VirtualGemsTelescope.G1,
      v2 -> VirtualGemsTelescope.G2,
      v3 -> VirtualGemsTelescope.G3,
      v4 -> VirtualGemsTelescope.G4
    ).mapFilter{ case (s, v) => s.map((_, v))}.toMap

    private def getCwfs[T: DetectorStateOps: Eq](getFollow: F[Boolean])
                                               (g: VirtualGemsTelescope, active: F[T])
    : F[GuiderConfig] = for {
      trk <- getNodChopTrackingConfig(epicsSys.gemsGuideConfig(g))
      fol <- getFollow.map{if(_) FollowOption.FollowOn else FollowOption.FollowOff}
      wfs <- active.map{x => if(DetectorStateOps.isActive(x)) GuiderSensorOn else GuiderSensorOff}
    } yield GuiderConfig(calcProbeTrackingConfig(fol, trk), wfs)

    private val getCwfs1: (VirtualGemsTelescope, F[Cwfs1DetectorState]) => F[GuiderConfig] =
      getCwfs(epicsSys.cwfs1Follow)

    private val getCwfs2: (VirtualGemsTelescope, F[Cwfs2DetectorState]) => F[GuiderConfig] =
      getCwfs(epicsSys.cwfs1Follow)

    private val getCwfs3: (VirtualGemsTelescope, F[Cwfs3DetectorState]) => F[GuiderConfig] =
      getCwfs(epicsSys.cwfs1Follow)

    private def getOdgw[T: DetectorStateOps: Eq](getParked: F[Boolean], getFollow: F[Boolean])
                                            (g: VirtualGemsTelescope, active: F[T])
    : F[GuiderConfig] = for {
      prk <- getParked
      trk <- getNodChopTrackingConfig(epicsSys.gemsGuideConfig(g))
      fol <- getFollow.map{ if(_) FollowOption.FollowOn else FollowOption.FollowOff}
      wfs <- active.map{x => if(DetectorStateOps.isActive[T](x)) GuiderSensorOn else GuiderSensorOff}
    } yield GuiderConfig(prk.fold(ProbeTrackingConfig.Parked, calcProbeTrackingConfig(fol, trk)), wfs)

    private val getOdgw1: (VirtualGemsTelescope, F[Odgw1DetectorState]) => F[GuiderConfig] =
      getOdgw(epicsSys.odgw1Parked, epicsSys.odgw1Follow)

    private val getOdgw2: (VirtualGemsTelescope, F[Odgw2DetectorState]) => F[GuiderConfig] =
      getOdgw(epicsSys.odgw2Parked, epicsSys.odgw2Follow)

    private val getOdgw3: (VirtualGemsTelescope, F[Odgw3DetectorState]) => F[GuiderConfig] =
      getOdgw(epicsSys.odgw3Parked, epicsSys.odgw3Follow)

    private val getOdgw4: (VirtualGemsTelescope, F[Odgw4DetectorState]) => F[GuiderConfig] =
      getOdgw(epicsSys.odgw4Parked, epicsSys.odgw4Follow)

    private def getInstrumentPorts: F[InstrumentPorts] = for {
      f2    <- epicsSys.f2Port.recover{case NullEpicsError(_) => InvalidPort}
      ghost <- epicsSys.ghostPort.recover{case NullEpicsError(_) => InvalidPort}
      gmos  <- epicsSys.gmosPort.recover{case NullEpicsError(_) => InvalidPort}
      gnirs <- epicsSys.gnirsPort.recover{case NullEpicsError(_) => InvalidPort}
      gpi   <- epicsSys.gpiPort.recover{case NullEpicsError(_) => InvalidPort}
      gsaoi <- epicsSys.gsaoiPort.recover{case NullEpicsError(_) => InvalidPort}
      nifs  <- epicsSys.nifsPort.recover{case NullEpicsError(_) => InvalidPort}
      niri  <- epicsSys.niriPort.recover{case NullEpicsError(_) => InvalidPort}
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

    override def retrieveConfigurationNorth(getAoFollow: F[Boolean]): F[TcsNorthControllerEpicsAo.EpicsTcsAoConfig] =
      for {
        base <- retrieveBaseConfiguration
        ao   <- getAowfs(getAoFollow)
      } yield TcsNorthControllerEpicsAo.EpicsTcsAoConfig(base, ao)

    private def retrieveGemsGuider(mapping: Map[GemsSource, VirtualGemsTelescope],
                                   gemsSource: GemsSource,
                                   getGuide: VirtualGemsTelescope => F[GuiderConfig]): F[GuiderConfig] =
      mapping.get(gemsSource).map(getGuide).getOrElse(GuiderConfig(ProbeTrackingConfig.Off, GuiderSensorOff).pure[F])

    override def retrieveConfigurationSouth(gemsSt: GemsWfsState[F]): F[TcsSouthControllerEpicsAo.EpicsTcsAoConfig] =
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

    override def retrieveBaseConfiguration: F[BaseEpicsTcsConfig] =
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
      } yield BaseEpicsTcsConfig(
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

  def apply[F[_]: MonadError[?[_], Throwable]](epicsSys: TcsEpics[F]): TcsConfigRetriever[F] =
    new TcsConfigRetrieverImpl(epicsSys)
}
