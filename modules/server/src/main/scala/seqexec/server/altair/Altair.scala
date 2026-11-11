// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.ApplicativeError
import cats.effect.Sync
import edu.gemini.spModel.gemini.altair.AltairConstants.FIELD_LENSE_PROP
import edu.gemini.spModel.gemini.altair.AltairConstants.GUIDESTAR_TYPE_PROP
import edu.gemini.spModel.gemini.altair.AltairParams.FieldLens
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import seqexec.model.`enum`.Instrument
import seqexec.model.enum.Resource
import seqexec.server.CleanConfig
import seqexec.server.ConfigUtilOps._
import seqexec.server.altair.AltairController._
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{ PauseConditionSet, ResumeConditionSet }
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import seqexec.server.tcs.TcsController.ProbeTrackingConfig
import squants.Time

trait Altair[F[_]] extends Gaos[F] {

  def pauseResume(
    config:        AltairConfig,
    currOffset:    FocalPlaneOffset,
    instrument:    Instrument,
    pauseReasons:  PauseConditionSet,
    resumeReasons: ResumeConditionSet
  ): F[AltairPauseResume[F]]

  val resource: Resource

  def usesP1(guide: AltairConfig): Boolean

  def usesOI(guide: AltairConfig): Boolean

  def isFollowing: F[Boolean]

  /**
   * AOWFS probe tracking demand for an Altair mode. `sequenceTracking` is what the step's
   * guideWithAO asks for; a mode may override it.
   *
   * This replaces the old `hasTarget: Boolean`, which could only pick between the sequence demand
   * and ProbeTrackingConfig.Off, and so had no way to express "keep following, don't nod/chop" --
   * the state the LGS relay modes need. See REL-4159.
   */
  def aoProbeTracking(
    guide:            AltairConfig,
    sequenceTracking: ProbeTrackingConfig
  ): ProbeTrackingConfig

}

object Altair {

  private class AltairImpl[F[_]: Sync](controller: AltairController[F], fieldLensIn: Boolean)
      extends Altair[F] {
    override def pauseResume(
      config:        AltairConfig,
      currOffset:    FocalPlaneOffset,
      instrument:    Instrument,
      pauseReasons:  PauseConditionSet,
      resumeReasons: ResumeConditionSet
    ): F[AltairPauseResume[F]] =
      controller.pauseResume(pauseReasons, resumeReasons, currOffset, instrument, fieldLensIn)(
        config
      )

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] =
      config.swap.map(controller.observe(expTime)(_)).getOrElse(Sync[F].unit)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] =
      config.swap.map(controller.endObserve).getOrElse(Sync[F].unit)

    override val resource: Resource = Resource.Altair

    override def usesP1(guide: AltairConfig): Boolean = guide match {
      case LgsWithP1 => true
      case _         => false
    }

    override def usesOI(guide: AltairConfig): Boolean = guide match {
      case LgsWithOi | Ngs(true, _) => true
      case _                        => false
    }

    override def isFollowing: F[Boolean] = controller.isFollowing

    // Ngs and plain Lgs: the AOWFS carries the NGS and tracks it normally, so the
    // step's demand stands. In particular it stands regardless of whether
    // STRAP/SFO happen to be closed right now -- conditioning this on the loop
    // state wrote aoFollow=Off and froze the probe (fixed and verified on-sky
    // 2026-07-17).
    //
    // LgsWithP1 / LgsWithOi are the relay modes: the tip-tilt reference lives on
    // P1 or the OIWFS. The AOWFS nod/chop must be AllOff (the TCS rejects
    // nod/chop on the AOWFS in P1+LGS mode), and the TCC does not put the
    // AOWFS/FSM into follow in these modes (confirmed on-sky 2026-07-28;
    // demanding follow On drove the probe on every offset and the TCS never
    // reached in-position). So the demand is Off: FollowOff + AllOff, matching
    // the state the TCC leaves it in. setAltairProbe never writes follow in
    // these modes, so this cannot freeze a probe the TCC has following, and
    // the AO loop is treated as guiding from the detector demand alone (see
    // TcsNorthControllerEpicsAo.aoGuideActive).
    //
    // This mirrors tcl/tk seqexec, whose beamconfig drives the AOWFS nod/chop
    // channel to all-Off when lgsp1 or lgsoi is set, and whose telconfig
    // inherits AO follow from the TCC rather than deciding it (RealTcs.itcl).
    override def aoProbeTracking(
      guide:            AltairConfig,
      sequenceTracking: ProbeTrackingConfig
    ): ProbeTrackingConfig = guide match {
      case Lgs(_, _, _)          => sequenceTracking
      case Ngs(_, _)             => sequenceTracking
      case LgsWithOi | LgsWithP1 => ProbeTrackingConfig.Off
      case AltairOff             => ProbeTrackingConfig.Off
    }

  }

  def apply[F[_]: Sync](controller: AltairController[F], fieldLensIn: Boolean): Altair[F] =
    new AltairImpl[F](controller, fieldLensIn)

  // Field lens state from the step's Altair component; a missing value is treated as OUT.
  def fieldLensIn(config: CleanConfig): Boolean =
    config
      .extractAOAs[FieldLens](FIELD_LENSE_PROP)
      .exists(_ == FieldLens.IN)

  def guideStarType[F[_]: ApplicativeError[*[_], Throwable]](
    config: CleanConfig
  ): F[GuideStarType] =
    config.extractAOAs[GuideStarType](GUIDESTAR_TYPE_PROP).toF[F]

}
