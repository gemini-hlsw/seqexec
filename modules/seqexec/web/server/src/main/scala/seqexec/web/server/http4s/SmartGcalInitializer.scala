// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.Sync
import edu.gemini.spModel.core.Peer
import edu.gemini.spModel.gemini.calunit.smartgcal.CalibrationProviderHolder
import edu.gemini.spModel.smartgcal.provider.CalibrationProviderImpl
import edu.gemini.spModel.smartgcal.repository.CalibrationUpdater
import edu.gemini.spModel.smartgcal.repository.CalibrationFileCache
import edu.gemini.spModel.smartgcal.repository.CalibrationRemoteRepository
import seqexec.model.config._

// This makes it cleaner that we have started SmartGcal
// Though in practice it can be bypassed with the Java API
sealed trait SmartGcal extends Product with Serializable {
  val cal: CalibrationUpdater
}

object SmartGcalInitializer {
  private final case class SmartGcalImpl(val cal: CalibrationUpdater) extends SmartGcal

  def init[F[_]: Sync](conf: SmartGcalConfiguration): F[SmartGcal] =
    Sync[F].delay {
      val peer       = new Peer(conf.smartGCalHost.renderString, 8443, edu.gemini.spModel.core.Site.GS)
      val calService = new CalibrationRemoteRepository(peer.host, peer.port)
      val cachedRepo = new CalibrationFileCache(conf.smartGCalDir.toFile)
      val provider   = new CalibrationProviderImpl(cachedRepo)
      CalibrationProviderHolder.setProvider(provider)
      CalibrationUpdater.instance.addListener(provider)
      CalibrationUpdater.instance.start(cachedRepo, calService, Int.MaxValue)
      SmartGcalImpl(CalibrationUpdater.instance)
    }
}
