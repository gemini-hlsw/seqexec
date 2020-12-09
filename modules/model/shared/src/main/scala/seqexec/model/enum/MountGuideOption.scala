// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import lucuma.core.util.Enumerated

/** Enumerated type for offloading of tip/tilt corrections from M2 to mount. */
sealed trait MountGuideOption extends Product with Serializable

object MountGuideOption {
  case object MountGuideOff extends MountGuideOption
  case object MountGuideOn  extends MountGuideOption

  /** @group Typeclass Instances */
  implicit val MountGuideOptionEnumerated: Enumerated[MountGuideOption] =
    Enumerated.of(MountGuideOff, MountGuideOn)
}
