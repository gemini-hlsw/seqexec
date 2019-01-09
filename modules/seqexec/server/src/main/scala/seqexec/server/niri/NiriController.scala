// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.Show
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.niri.NiriController.{DCConfig, NiriConfig}
import seqexec.server.{ObserveCommand, Progress, SeqAction}
import squants.Time

trait NiriController {

  def applyConfig(config: NiriConfig): SeqAction[Unit]

  def observe(fileId: ImageFileId, cfg: DCConfig): SeqAction[ObserveCommand.Result]

  def endObserve: SeqAction[Unit]

  def stopObserve: SeqAction[Unit]

  def abortObserve: SeqAction[Unit]

  def observeProgress(total: Time): fs2.Stream[IO, Progress]

  def calcTotalExposureTime(cfg: DCConfig): Time

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