// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.math.Offset

/**
 * Additional configuration information for [[gem.Step.Science Science]] steps.
 * @group Configurations
 */
final case class TelescopeConfig(p: Offset.P, q: Offset.Q) {
  def offset: Offset = Offset(p, q)
}

object TelescopeConfig extends ((Offset.P, Offset.Q) => TelescopeConfig) {
  val Zero: TelescopeConfig = TelescopeConfig(Offset.P.Zero, Offset.Q.Zero)
}
