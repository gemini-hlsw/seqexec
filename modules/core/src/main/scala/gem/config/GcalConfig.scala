package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalShutter}

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, shutter: GcalShutter) {
  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: ISet[GcalArc] =
    lamp.fold(_ => ISet.empty[GcalArc], as => as.tail.insert(as.head))
}

object GcalConfig {
  type GcalArcs = OneAnd[ISet, GcalArc]
  type GcalLamp = GcalContinuum \/ GcalArcs

  def mkLamp(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): Option[GcalLamp] =
    continuum.map(_.left[GcalArcs]).orElse(arcs.filter(_._2).unzip._1.toList match {
      case h :: t => Some(OneAnd(h, ISet.fromList(t)).right[GcalContinuum])
      case _      => None
    })

  def unsafeMkLamp(co: Option[GcalContinuum], as: (GcalArc, Boolean)*): GcalLamp =
    mkLamp(co, as: _*).getOrElse(sys.error("no Gcal continuum nor arc lamps"))
}

