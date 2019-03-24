// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.telnetd

final case class TelnetdConfiguration(port: Int)

object TelnetdConfiguration {

  val forTesting: TelnetdConfiguration =
    apply(9091)

}
