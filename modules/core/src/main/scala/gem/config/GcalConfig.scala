package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalShutter}

import scalaz.\/

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, shutter: GcalShutter)

object GcalConfig {
  type GcalLamp = GcalContinuum \/ Set[GcalArc]
}

