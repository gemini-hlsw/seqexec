package gem
package config

import gem.enum.{GCalArc, GCalContinuum, GCalShutter}

import scalaz.\/

import GcalConfig.GCalLamp

case class GcalConfig(lamp: GCalLamp, shutter: GCalShutter)

object GcalConfig {
  type GCalLamp = GCalContinuum \/ Set[GCalArc]
}

