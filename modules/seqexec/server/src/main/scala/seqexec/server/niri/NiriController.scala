// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.Show
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.niri.NiriController.{DCConfig, NiriConfig}
import seqexec.server.Progress
import squants.Time

trait NiriController[F[_]] {

  def applyConfig(config: NiriConfig): F[Unit]

  def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommandResult]

  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def observeProgress(total: Time): fs2.Stream[F, Progress]

  def calcTotalExposureTime(cfg: DCConfig): F[Time]

}

object NiriController {
  type ExposureTime = Time
  type Coadds = Int
  type Camera = edu.gemini.spModel.gemini.niri.Niri.Camera
  type BeamSplitter = edu.gemini.spModel.gemini.niri.Niri.BeamSplitter
  type BuiltInROI = edu.gemini.spModel.gemini.niri.Niri.BuiltinROI
  type Filter = edu.gemini.spModel.gemini.niri.Niri.Filter
  type Focus = edu.gemini.spModel.gemini.niri.Niri.Focus
  type Disperser = edu.gemini.spModel.gemini.niri.Niri.Disperser
  type Mask = edu.gemini.spModel.gemini.niri.Niri.Mask
  type ReadMode = edu.gemini.seqexec.server.niri.ReadMode

  final case class DCConfig(exposureTime: ExposureTime,
                            coadds: Coadds,
                            readMode: ReadMode,
                            builtInROI: BuiltInROI
                           )
  sealed trait CCConfig

  final case class Common(camera: Camera,
                          beamSplitter: BeamSplitter,
                          focus: Focus,
                          disperser: Disperser,
                          mask: Mask
                         )

  // All components are included because the instrument scientist requested that every component be
  // configured for Darks
  final case class Dark(common: Common) extends CCConfig

  final case class Illuminated(filter: Filter, common: Common) extends CCConfig

  final case class NiriConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[NiriConfig] = Show.fromToString

}
