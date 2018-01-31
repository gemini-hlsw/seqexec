// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.gnirs.GnirsController.GnirsConfig
import edu.gemini.seqexec.server.{InstrumentControllerSim, ObserveCommand, SeqAction}
import squants.Time

import scalaz.Show

object GnirsControllerSim extends GnirsController {

  implicit val configShow: Show[GnirsConfig] = Show.shows { config =>
    s"(${config.cc.mode}, ${config.cc.camera}, ${config.cc.centralWavelength}, ${config.cc.decker}, " +
      s"${config.cc.disperser},${config.cc.filter}, ${config.cc.pixelScale}, ${config.cc.slitWidth}, " +
      s"${config.cc.wollanstonPrism}, ${config.dc.exposureTime}, ${config.dc.coadds}, ${config.dc.readMode}, " +
      s"${config.dc.wellDepth})"
  }

  private val sim: InstrumentControllerSim = InstrumentControllerSim(s"GNIRS")

  override def observe(obsid: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] =
    sim.observe(obsid, expTime)

  override def applyConfig(config: GnirsConfig): SeqAction[Unit] = sim.applyConfig(config)

  override def stopObserve: SeqAction[Unit] = sim.stopObserve

  override def abortObserve: SeqAction[Unit] = sim.abortObserve

  override def endObserve: SeqAction[Unit] = sim.endObserve

}
