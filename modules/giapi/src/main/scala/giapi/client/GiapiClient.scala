// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Show
import edu.gemini.aspen.giapi.commands.{Activity, SequenceCommand}
import giapi.client.commands.{Command, CommandResult, Configuration}

import scala.concurrent.duration._

trait GiapiClient[F[_]] {
  def giapi: Giapi[F]

  def genericApply(configuration: Configuration): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        configuration
      ), GiapiClient.DefaultCommandTimeout)

  def observe[A: Show](dataLabel: A, expTime: FiniteDuration): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.OBSERVE,
        Activity.PRESET_START,
        Configuration.single(commands.DataLabelCfg, dataLabel)
      ), expTime)
}

object GiapiClient {
  // GPI documentation specify 60 seconds as the max time to move mechanism.
  val DefaultCommandTimeout: FiniteDuration = 60.seconds
}
