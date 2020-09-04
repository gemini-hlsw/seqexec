// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats._

/**
  * Indicates how each subsystems is treated, e.g. full connection or simulated
  */
final case class SystemsControlConfiguration(
  altair:   ControlStrategy,
  gems:     ControlStrategy,
  dhs:      ControlStrategy,
  f2:       ControlStrategy,
  gcal:     ControlStrategy,
  gmos:     ControlStrategy,
  gnirs:    ControlStrategy,
  gpi:      ControlStrategy,
  gpiGds:   ControlStrategy,
  ghost:    ControlStrategy,
  ghostGds: ControlStrategy,
  gsaoi:    ControlStrategy,
  gws:      ControlStrategy,
  nifs:     ControlStrategy,
  niri:     ControlStrategy,
  tcs:      ControlStrategy
) {
  def connectEpics: Boolean =
    (altair.connect || gems.connect || f2.connect || gcal.connect || gmos.connect || gnirs.connect || gsaoi.connect || gws.connect || nifs.connect || niri.connect || tcs.connect)
}

object SystemsControlConfiguration {
  implicit val eqSystemsControl: Eq[SystemsControlConfiguration] =
    Eq.by(
      x =>
        (x.altair,
         x.gems,
         x.dhs,
         x.f2,
         x.gcal,
         x.ghost,
         x.gmos,
         x.gnirs,
         x.gpi,
         x.gpiGds,
         x.ghostGds,
         x.gsaoi,
         x.gws,
         x.nifs,
         x.niri,
         x.tcs)
    )

}
