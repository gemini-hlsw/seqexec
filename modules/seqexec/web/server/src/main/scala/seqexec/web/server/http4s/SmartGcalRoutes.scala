// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._

import edu.gemini.spModel.core.Peer
import edu.gemini.spModel.gemini.calunit.smartgcal.CalibrationProviderHolder
import edu.gemini.spModel.smartgcal.provider.CalibrationProviderImpl
import edu.gemini.spModel.smartgcal.repository.CalibrationUpdater
import edu.gemini.spModel.smartgcal.repository.CalibrationFileCache
import edu.gemini.spModel.smartgcal.repository.CalibrationRemoteRepository
import java.nio.file.Paths

/**
  * Rest Endpoints for SmartGceal
  */
class SmartGcalRoutes(smartGCalHost: String, smartGCalLocation: String) {

  val peer       = new Peer(smartGCalHost, 8443, edu.gemini.spModel.core.Site.GS)
  val calService = new CalibrationRemoteRepository(peer.host, peer.port)
  val cachedRepo = new CalibrationFileCache(Paths.get(smartGCalLocation).toFile)
  val provider   = new CalibrationProviderImpl(cachedRepo)
  CalibrationProviderHolder.setProvider(provider)
  CalibrationUpdater.instance.addListener(provider)
  CalibrationUpdater.instance.start(cachedRepo, calService, Int.MaxValue)

  val publicService: HttpRoutes[IO] = HttpRoutes.of {

    case POST -> Root / "refresh" =>
      IO(CalibrationUpdater.instance.updateNowInBackground()) *>
        Ok("Smart GCal Refresh started")

  }

  def service: HttpRoutes[IO] = publicService
}
