// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import cats.data.NonEmptySet
import cats.effect.IO
import monocle.Lens
import mouse.boolean._
import monocle.macros.Lenses
import org.log4s.{Logger, getLogger}
import seqexec.server.EpicsCodex.encode
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.tcs.GemsSource._
import seqexec.server.tcs.TcsController.{GuiderConfig, ProbeTrackingConfig, Subsystem, wavelengthEq}
import seqexec.server.tcs.TcsControllerEpicsCommon.{BaseEpicsTcsConfig, GuideControl, agTimeout, applyParam, encodeFollowOption, setGuideProbe, setHrPickup, setNodChopProbeTrackingConfig, setOiwfsProbe, setPwfs1Probe, setScienceFold, setTelescopeOffset, setWavelength, tcsTimeout}
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, VirtualGemsTelescope}
import seqexec.server.tcs.TcsSouthController.{GemsGuiders, TcsSouthAoConfig}

object TcsSouthControllerEpicsAo {

  val Log: Logger = getLogger

  def setNgsGuide(followCmd: ProbeFollowCmd[IO], l: Lens[EpicsTcsAoConfig, GuiderConfig])(
    g: VirtualGemsTelescope,
    subsystems: NonEmptySet[Subsystem],
    current: ProbeTrackingConfig,
    demand: ProbeTrackingConfig
  ): Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    if (subsystems.contains(Subsystem.Gaos)) {
      val actions = List(
        (current.getNodChop =!= demand.getNodChop)
          .option(setNodChopProbeTrackingConfig(TcsEpics.instance.gemsProbeGuideCmd(g))(demand.getNodChop)),
        (current.follow =!= demand.follow).option(followCmd.setFollowState(encode(demand.follow)))
      ).flattenOption

      actions.nonEmpty.option { x =>
        actions.sequence *>
          IO((l ^|-> GuiderConfig.tracking).set(demand)(x))
      }
    }
    else none

  val setNgs1Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
    Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setNgsGuide(TcsEpics.instance.ngs1ProbeFollowCmd, EpicsTcsAoConfig.cwfs1)

  val setNgs2Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
    Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setNgsGuide(TcsEpics.instance.ngs2ProbeFollowCmd, EpicsTcsAoConfig.cwfs2)

  val setNgs3Guide: (VirtualGemsTelescope, NonEmptySet[Subsystem], ProbeTrackingConfig, ProbeTrackingConfig) =>
    Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setNgsGuide(TcsEpics.instance.ngs3ProbeFollowCmd, EpicsTcsAoConfig.cwfs3)

  private def odgw1GuiderControl(g: VirtualGemsTelescope): GuideControl[IO] = GuideControl(Subsystem.Gaos, TcsEpics.instance.odgw1ParkCmd,
    TcsEpics.instance.gemsProbeGuideCmd(g), TcsEpics.instance.odgw1FollowCmd)

  def setOdgw1Probe(g: VirtualGemsTelescope)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setGuideProbe(odgw1GuiderControl(g), (EpicsTcsAoConfig.odgw1 ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def odgw2GuiderControl(g: VirtualGemsTelescope): GuideControl[IO] = GuideControl(Subsystem.Gaos, TcsEpics.instance.odgw2ParkCmd,
    TcsEpics.instance.gemsProbeGuideCmd(g), TcsEpics.instance.odgw2FollowCmd)

  def setOdgw2Probe(g: VirtualGemsTelescope)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setGuideProbe(odgw2GuiderControl(g), (EpicsTcsAoConfig.odgw2 ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def odgw3GuiderControl(g: VirtualGemsTelescope): GuideControl[IO] = GuideControl(Subsystem.Gaos, TcsEpics.instance.odgw3ParkCmd,
    TcsEpics.instance.gemsProbeGuideCmd(g), TcsEpics.instance.odgw3FollowCmd)

  def setOdgw3Probe(g: VirtualGemsTelescope)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setGuideProbe(odgw3GuiderControl(g), (EpicsTcsAoConfig.odgw3 ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def odgw4GuiderControl(g: VirtualGemsTelescope): GuideControl[IO] = GuideControl(Subsystem.Gaos, TcsEpics.instance.odgw4ParkCmd,
    TcsEpics.instance.gemsProbeGuideCmd(g), TcsEpics.instance.odgw4FollowCmd)

  def setOdgw4Probe(g: VirtualGemsTelescope)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    setGuideProbe(odgw4GuiderControl(g), (EpicsTcsAoConfig.odgw4 ^|-> GuiderConfig.tracking).set)(a, b, c)

  def setGemsProbes(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: GemsGuiders)
  : List[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] = List(
      current.mapping.get(Ttgs1).flatMap(setNgs1Guide(_, subsystems, current.cwfs1.tracking, demand.ngs1.tracking)),
      current.mapping.get(Ttgs2).flatMap(setNgs1Guide(_, subsystems, current.cwfs2.tracking, demand.ngs2.tracking)),
      current.mapping.get(Ttgs3).flatMap(setNgs1Guide(_, subsystems, current.cwfs3.tracking, demand.ngs3.tracking)),
      current.mapping.get(Odgw1).flatMap(setOdgw1Probe(_)(subsystems, current.odgw1.tracking, demand.odgw1.tracking)),
      current.mapping.get(Odgw2).flatMap(setOdgw1Probe(_)(subsystems, current.odgw2.tracking, demand.odgw2.tracking)),
      current.mapping.get(Odgw3).flatMap(setOdgw1Probe(_)(subsystems, current.odgw3.tracking, demand.odgw3.tracking)),
      current.mapping.get(Odgw4).flatMap(setOdgw1Probe(_)(subsystems, current.odgw4.tracking, demand.odgw4.tracking))
    ).flattenOption

  def applyAoConfig(subsystems: NonEmptySet[Subsystem],
                  gaos: Gems[IO],
                  tcs: TcsSouthAoConfig): IO[Unit] = {
    def configParams(current: EpicsTcsAoConfig): List[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
      List(
        setPwfs1Probe(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.tracking, tcs.gds.pwfs1.tracking),
        setOiwfsProbe(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.tracking, tcs.gds.oiwfs.tracking),
        tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.base.offset,
          o.toFocalPlaneOffset(current.base.iaa), setTelescopeOffset, EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.offset
        )),
        tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.base.wavelA, _, setWavelength,
          EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.wavelA
        )),
        setScienceFold(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc.sfPos),
        setHrPickup(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc)
      ).flattenOption ++ setGemsProbes(subsystems, current, tcs.gds.aoguide)

    def sysConfig(current: EpicsTcsAoConfig): IO[EpicsTcsAoConfig] = {
      val params = configParams(current)

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p) }
          _ <- TcsEpics.instance.post
          _ <- IO(Log.debug("TCS configuration command post"))
          _ <- if(subsystems.contains(Subsystem.Mount))
            TcsEpics.instance.waitInPosition(tcsTimeout) *> IO.apply(Log.info("TCS inposition"))
          else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
            TcsEpics.instance.waitAGInPosition(agTimeout) *> IO.apply(Log.debug("AG inposition"))
          else IO.unit
        } yield s
      else
        IO(Log.debug("Skipping TCS configuration")) *> IO(current)
    }

    //TODO add guideOff/guideOn
    //TODO add GeMS pause/resume
    for {
      s0 <- TcsConfigRetriever.retrieveConfigurationSouth(gaos.stateGetter)
      _  <- if(s0.base.useAo) IO.unit
            else SeqexecFailure.Execution("Found useAo not set for AO step.").raiseError[IO, Unit]
      _  <- sysConfig(s0)
    } yield ()
  }

  @Lenses
  final case class EpicsTcsAoConfig(
    base: BaseEpicsTcsConfig,
    mapping: Map[GemsSource, VirtualGemsTelescope],
    cwfs1: GuiderConfig,
    cwfs2: GuiderConfig,
    cwfs3: GuiderConfig,
    odgw1: GuiderConfig,
    odgw2: GuiderConfig,
    odgw3: GuiderConfig,
    odgw4: GuiderConfig
  )
}
