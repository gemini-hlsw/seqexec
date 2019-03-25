// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.main

import gem.web.WebConfiguration
import gem.dao.DatabaseConfiguration
import gem.telnetd.TelnetdConfiguration

final case class MainConfiguration(
  database: DatabaseConfiguration,
  web:      WebConfiguration,
  telnetd:  TelnetdConfiguration
)

object MainConfiguration {

  val forTesting: MainConfiguration =
    apply(
      DatabaseConfiguration.forTesting,
      WebConfiguration.forTesting,
      TelnetdConfiguration.forTesting
    )

}