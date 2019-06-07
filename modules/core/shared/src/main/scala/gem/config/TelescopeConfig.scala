// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gsp.math.Offset

import cats.Order
import cats.implicits._
import monocle._


/**
 * Additional configuration information for [[gem.Step.Science Science]] steps.
 * @group Configurations
 */
final case class TelescopeConfig(p: Offset.P, q: Offset.Q) {
  def offset: Offset = Offset(p, q)
}

object TelescopeConfig extends ((Offset.P, Offset.Q) => TelescopeConfig) with TelescopeConfigOptics {
  val Zero: TelescopeConfig = TelescopeConfig(Offset.P.Zero, Offset.Q.Zero)

  implicit val OrderTelescopeConfig: Order[TelescopeConfig] =
    Order.by(t => (t.p, t.q))

}

trait TelescopeConfigOptics {

  /** @group Optics */
  val p: Lens[TelescopeConfig, Offset.P] =
    Lens[TelescopeConfig, Offset.P](_.p)(a => _.copy(p = a))

  /** @group Optics */
  val q: Lens[TelescopeConfig, Offset.Q] =
    Lens[TelescopeConfig, Offset.Q](_.q)(a => _.copy(q = a))

}
