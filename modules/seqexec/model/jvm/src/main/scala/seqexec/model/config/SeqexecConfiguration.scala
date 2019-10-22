// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import cats.implicits._
import gem.enum.Site

final case class SeqexecConfiguration(
  site:      Site,
  mode:      Mode,
  seqexecEngine: SeqexecServerConfiguration,
  webServer: WebServerConfiguration,
  smartGcal: SmartGcalConfiguration,
  authentication: AuthenticationConfig
)

object SeqexecConfiguration {
  implicit val eqSeqexecConfiguration: Eq[SeqexecConfiguration] =
    Eq.by(x => (x.site, x.mode, x.seqexecEngine, x.webServer, x.smartGcal, x.authentication))
}
