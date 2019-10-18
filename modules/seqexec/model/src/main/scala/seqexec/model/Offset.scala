// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import gsp.math.Axis
import seqexec.model.enum.SystemName

object OffsetType {
  type Telescope
  type NSNodA
  type NSNodB
}

sealed trait OffsetShow[A] {
  val show: String
}
object OffsetShow {
  implicit object OffsetNSNodAShow extends OffsetShow[OffsetType.NSNodA] {
    override val show = "A"
  }

  implicit object OffsetNSNodBShow extends OffsetShow[OffsetType.NSNodB] {
    override val show = "B"
  }

  implicit object OffsetAxisPShow extends OffsetShow[Axis.P] {
    override val show = "p"
  }

  implicit object OffsetAxisQShow extends OffsetShow[Axis.Q] {
    override val show = "q"
  }
}

sealed trait OffsetConfigResolver[T, A] {
  val systemName: SystemName
  val configItem: String
}
object OffsetConfigResolver {
  sealed trait TelescopeOffsetConfigResolver[A]
    extends OffsetConfigResolver[OffsetType.Telescope, A] {
    val systemName = SystemName.Telescope
  }
  implicit object TelescopeOffsetConfigResolverP
    extends TelescopeOffsetConfigResolver[Axis.P] {
    val configItem = "p"
  }
  implicit object TelescopeOffsetConfigResolverQ
    extends TelescopeOffsetConfigResolver[Axis.Q] {
    val configItem = "q"
  }

  sealed trait NSOffsetConfigResolver[T, A]
    extends OffsetConfigResolver[T, A] {
    val systemName = SystemName.Instrument
  }
  sealed trait NSOffsetConfigResolverA[A]
    extends NSOffsetConfigResolver[OffsetType.NSNodA, A]
  implicit object NSOffsetConfigResolverAP
    extends NSOffsetConfigResolverA[Axis.P] {
    val configItem = "nsBeamA-p"
  }
  implicit object NSOffsetConfigResolverAQ
    extends NSOffsetConfigResolverA[Axis.Q] {
    val configItem = "nsBeamA-q"
  }
  sealed trait NSOffsetConfigResolverB[A]
    extends NSOffsetConfigResolver[OffsetType.NSNodB, A]
  implicit object NSOffsetConfigResolverBP
    extends NSOffsetConfigResolverB[Axis.P] {
    val configItem = "nsBeamB-p"
  }
  implicit object NSOffsetConfigResolverBQ
    extends NSOffsetConfigResolverB[Axis.Q] {
    val configItem = "nsBeamB-q"
  }
}
