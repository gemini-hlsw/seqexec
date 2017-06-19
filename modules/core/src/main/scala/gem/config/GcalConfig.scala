/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}

import java.time.Duration

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, filter: GcalFilter, diffuser: GcalDiffuser, shutter: GcalShutter, exposureTime: Duration, coadds: Short) {
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
