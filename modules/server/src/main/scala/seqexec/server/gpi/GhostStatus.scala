// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

object GhostStatus {
  val statusesToMonitor: List[String] = List(
    "ghost:sad:dc:red.progress",
    "ghost:sad:dc:red.target",
    "ghost:sad:dc:red.readout_target",
    "ghost:sad:dc:red.command_state"
  )
}
