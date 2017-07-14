// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}

import java.time.Duration

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

/**
 * Additional configuration information for [[gem.Step.Gcal Gcal]] steps.
 * @group Configurations
 */
final case class GcalConfig(lamp: GcalLamp, filter: GcalFilter, diffuser: GcalDiffuser, shutter: GcalShutter, exposureTime: Duration, coadds: Short) {
  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: ISet[GcalArc] =
    lamp.fold(_ => ISet.empty[GcalArc], _.toISet)
}

object GcalConfig {
  // We make this a sealed abstract case class in order to force usage of the
  // companion object constructor.  The OneAnd head is guaranteed to always be
  // the minimum GcalArc in the group.
  sealed abstract case class GcalArcs(arcs: OneAnd[ISet, GcalArc]) {
    def toList: List[GcalArc] =
      arcs.head :: arcs.tail.toList

    def toISet: ISet[GcalArc] =
      arcs.tail.insert(arcs.head)
  }

  object GcalArcs {
    /** Constructs GcalArcs such that the GcalArc instances are always in order.
      */
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    def apply(arc0: GcalArc, arcs: List[GcalArc]): GcalArcs = {
      val all = ISet.fromList(arc0 :: arcs)
      new GcalArcs(OneAnd(all.elemAt(0).get, all.deleteAt(0))) {}
    }
  }

  type GcalLamp = GcalContinuum \/ GcalArcs

  object GcalLamp {
    def fromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): Option[GcalLamp] = {
      // Extract the arc lamps to include, if any.
      val as = arcs.filter(_._2).unzip._1.toList

      // Extract the continuum lamp, assuming there are no arcs.
      val co = continuum.flatMap { c => as.isEmpty option c.left[GcalArcs] }

      // Prepare the arc lamps, assuming there is no continuum.
      val ao = as match {
        case h :: t if continuum.isEmpty => Some(GcalArcs(h, t).right[GcalContinuum])
        case _                           => None
      }

      co orElse ao
    }

    def fromContinuum(continuum: GcalContinuum): GcalLamp =
      continuum.left

    def fromArcs(arc0: GcalArc, arcs: GcalArc*): GcalLamp =
      GcalArcs(arc0, arcs.toList).right

    def unsafeFromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): GcalLamp =
      fromConfig(continuum, arcs: _*).getOrElse {
        sys.error(s"misconfigured Gcal lamps: continuum=$continuum, arcs=[${arcs.filter(_._2).unzip._1.mkString(",")}]")
      }
  }
}
