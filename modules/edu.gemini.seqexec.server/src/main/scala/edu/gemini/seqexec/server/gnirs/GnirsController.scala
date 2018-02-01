// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.gnirs.GnirsController.GnirsConfig
import edu.gemini.seqexec.server.{ObserveCommand, SeqAction}
import squants.Time

trait GnirsController {

  def applyConfig(config: GnirsConfig): SeqAction[Unit]

  def observe(obsid: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: SeqAction[Unit]

  def stopObserve: SeqAction[Unit]

  def abortObserve: SeqAction[Unit]

}

object GnirsController {

  sealed trait Mode

  case object Acquisition extends Mode
  sealed abstract class Spectrography(val disperser: Disperser) extends Mode
  final case class CrossDisperserS(override val disperser: Disperser) extends Spectrography(disperser)
  final case class CrossDisperserL(override val disperser: Disperser) extends Spectrography(disperser)
  final case class Wollaston(override val disperser: Disperser) extends Spectrography(disperser)

  type Camera = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Camera

  type CentralWavelength = edu.gemini.spModel.core.Wavelength

  type Coadds = Int

  type Decker = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Decker

  type Disperser = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Disperser

  type ExposureTime = Time

  type Filter = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Filter

  type PixelScale = edu.gemini.spModel.gemini.gnirs.GNIRSParams.PixelScale

  type ReadMode = edu.gemini.spModel.gemini.gnirs.GNIRSParams.ReadMode

  type SlitWidth = edu.gemini.spModel.gemini.gnirs.GNIRSParams.SlitWidth

  type WellDepth = edu.gemini.spModel.gemini.gnirs.GNIRSParams.WellDepth

  type WollanstonPrism = edu.gemini.spModel.gemini.gnirs.GNIRSParams.WollastonPrism

  final case class DCConfig(exposureTime: ExposureTime,
                            coadds: Coadds,
                            readMode: ReadMode,
                            wellDepth: WellDepth
                           )

  final case class CCConfig(mode: Mode,
                            camera: Camera,
                            centralWavelength: CentralWavelength,
                            decker: Option[Decker],
                            disperser: Disperser,
                            filter: Option[Filter],
                            pixelScale: PixelScale,
                            slitWidth: SlitWidth,
                            wollanstonPrism: WollanstonPrism
                           )

  final case class GnirsConfig(cc: CCConfig, dc: DCConfig)

}