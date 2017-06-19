// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}

final case class TelescopeConfig(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object TelescopeConfig extends ((OffsetP, OffsetQ) => TelescopeConfig) {
  val Zero = TelescopeConfig(OffsetP.Zero, OffsetQ.Zero)
}

