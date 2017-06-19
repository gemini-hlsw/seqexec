package gem
package config

import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}

final case class TelescopeConfig(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object TelescopeConfig extends ((OffsetP, OffsetQ) => TelescopeConfig) {
  val Zero = TelescopeConfig(OffsetP.Zero, OffsetQ.Zero)
}

