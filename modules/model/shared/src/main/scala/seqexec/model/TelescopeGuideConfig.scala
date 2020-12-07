// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import monocle.macros.Lenses
import seqexec.model.enum.MountGuideOption

/** Data type for guide config. */
@Lenses
final case class TelescopeGuideConfig(mountGuide: MountGuideOption,
                                      m1Guide:    M1GuideConfig,
                                      m2Guide:    M2GuideConfig)

object TelescopeGuideConfig {
  implicit val eq: Eq[TelescopeGuideConfig] =
    Eq.by(x => (x.mountGuide, x.m1Guide, x.m2Guide))
}
