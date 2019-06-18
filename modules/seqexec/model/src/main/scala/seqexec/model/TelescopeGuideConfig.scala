// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.M1GuideConfig
import seqexec.model.enum.M2GuideConfig

/** Data type for guide config. */
@Lenses
final case class TelescopeGuideConfig(mountGuide: MountGuideOption,
                                      m1Guide:    M1GuideConfig,
                                      m2Guide:    M2GuideConfig)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TelescopeGuideConfig {
  implicit val eq: Eq[TelescopeGuideConfig] =
    Eq.by(x => (x.mountGuide, x.m1Guide, x.m2Guide))
}
