// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import gem.util.Enumerated

trait GsaoiGuider[F[_]] {
  import GsaoiGuider._
  def currentState: F[GuideState]
  def guide: F[Unit]
  def endGuide: F[Unit]
}

object GsaoiGuider {

  sealed trait OdgwId extends Product with Serializable

  object OdgwId {
    case object Odgw1 extends OdgwId
    case object Odgw2 extends OdgwId
    case object Odgw3 extends OdgwId
    case object Odgw4 extends OdgwId

    implicit val odgwIdEnumerated: Enumerated[OdgwId] = Enumerated.of(Odgw1, Odgw2, Odgw3, Odgw4)

  }

  trait GuideState {
    def isGuideActive: Boolean
    def isOdgwGuiding(odgwId: OdgwId): Boolean
  }
}
