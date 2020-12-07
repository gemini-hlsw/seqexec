// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import lucuma.core.enum.Site

/**
 * Top configuration of the seqexec
 * @param site Site this seqexec instance handles (GN/GS)
 * @param mode Execution mode
 * @param seqexecEngine Configuration of the engine
 * @param webServer Web side configuration
 * @param smartGcal Configuration to reach SmartGCal
 * @param authentication Configuration to support authentication
 */
final case class SeqexecConfiguration(
  site:           Site,
  mode:           Mode,
  seqexecEngine:  SeqexecEngineConfiguration,
  webServer:      WebServerConfiguration,
  smartGcal:      SmartGcalConfiguration,
  authentication: AuthenticationConfig
)

object SeqexecConfiguration {
  implicit val eqSeqexecConfiguration: Eq[SeqexecConfiguration] =
    Eq.by(x => (x.site, x.mode, x.seqexecEngine, x.webServer, x.smartGcal, x.authentication))
}
