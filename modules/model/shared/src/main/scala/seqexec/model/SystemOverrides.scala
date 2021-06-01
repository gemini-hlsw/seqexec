// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._

case class SystemOverrides(
  isTcsEnabled:        Boolean,
  isInstrumentEnabled: Boolean,
  isGcalEnabled:       Boolean,
  isDhsEnabled:        Boolean
) {
  def disableTcs: SystemOverrides = copy(isTcsEnabled = false)

  def enableTcs: SystemOverrides = copy(isTcsEnabled = true)

  def disableInstrument: SystemOverrides = copy(isInstrumentEnabled = false)

  def enableInstrument: SystemOverrides = copy(isInstrumentEnabled = true)

  def disableGcal: SystemOverrides = copy(isGcalEnabled = false)

  def enableGcal: SystemOverrides = copy(isGcalEnabled = true)

  def disableDhs: SystemOverrides = copy(isDhsEnabled = false)

  def enableDhs: SystemOverrides = copy(isDhsEnabled = true)
}

object SystemOverrides {
  val AllEnabled: SystemOverrides = SystemOverrides(
    isTcsEnabled = true,
    isInstrumentEnabled = true,
    isGcalEnabled = true,
    isDhsEnabled = true
  )

  implicit val eqSystemOverrrides: Eq[SystemOverrides] =
    Eq.by(x => (x.isTcsEnabled, x.isInstrumentEnabled, x.isGcalEnabled, x.isDhsEnabled))

}
