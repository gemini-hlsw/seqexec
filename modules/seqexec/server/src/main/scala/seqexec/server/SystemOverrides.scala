// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import io.chrisdavenport.log4cats.Logger

trait SystemOverrides {
  val isTcsEnabled: Boolean
  val isInstrumentEnabled: Boolean
  val isGcalEnabled: Boolean
  val isDhsEnabled: Boolean
  def disableTcs: SystemOverrides
  def enableTcs: SystemOverrides
  def disableInstrument: SystemOverrides
  def enableInstrument: SystemOverrides
  def disableGcal: SystemOverrides
  def enableGcal: SystemOverrides
  def disableDhs: SystemOverrides
  def enableDhs: SystemOverrides
}

object SystemOverrides {
  val AllEnabled: SystemOverrides = SystemOverridesImpl(
    isTcsEnabled = true,
    isInstrumentEnabled = true,
    isGcalEnabled = true,
    isDhsEnabled = true
  )

  private case class SystemOverridesImpl(override val isTcsEnabled: Boolean,
                                         override val isInstrumentEnabled: Boolean,
                                         override val isGcalEnabled: Boolean,
                                         override val isDhsEnabled: Boolean
  ) extends SystemOverrides {
    override def disableTcs: SystemOverrides = copy(isTcsEnabled = false)

    override def enableTcs: SystemOverrides = copy(isTcsEnabled = true)

    override def disableInstrument: SystemOverrides = copy(isInstrumentEnabled = false)

    override def enableInstrument: SystemOverrides = copy(isInstrumentEnabled = true)

    override def disableGcal: SystemOverrides = copy(isGcalEnabled = false)

    override def enableGcal: SystemOverrides = copy(isGcalEnabled = true)

    override def disableDhs: SystemOverrides = copy(isDhsEnabled = false)

    override def enableDhs: SystemOverrides = copy(isDhsEnabled = true)
  }

  def overrideLogMessage[F[_]: Logger](systemName: String, op: String): F[Unit] =
    Logger[F].info(s"System $systemName overridden. Operation $op skipped.")
}
