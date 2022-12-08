// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Applicative
import cats.syntax.all._
import scala.concurrent.duration._

import edu.gemini.aspen.giapi.commands.Activity
import edu.gemini.aspen.giapi.commands.SequenceCommand
import giapi.client.commands.Command
import giapi.client.commands.CommandResult
import giapi.client.commands.Configuration
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response

/////////////////////////////////////////////////////////////////
// The GiapiClient comprises the common commands for such clients
/////////////////////////////////////////////////////////////////
trait GiapiClient[F[_]] {
  import GiapiClient.DefaultCommandTimeout

  def giapi: Giapi[F]

  def test: F[CommandResult] =
    giapi.command(Command(SequenceCommand.TEST, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def init: F[CommandResult] =
    giapi.command(Command(SequenceCommand.INIT, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def datum: F[CommandResult] =
    giapi.command(Command(SequenceCommand.DATUM, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def park: F[CommandResult] =
    giapi.command(Command(SequenceCommand.PARK, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def verify: F[CommandResult] =
    giapi.command(Command(SequenceCommand.VERIFY, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def endVerify: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_VERIFY, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def guide: F[CommandResult] =
    giapi.command(Command(SequenceCommand.GUIDE, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def endGuide: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_GUIDE, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def observe[A: GiapiConfig](dataLabel: A, timeout: FiniteDuration): F[CommandResult] =
    giapi.command(Command(
                    SequenceCommand.OBSERVE,
                    Activity.PRESET_START,
                    Configuration.single(commands.DataLabelCfg, dataLabel)
                  ),
                  timeout
    )

  def endObserve: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_OBSERVE, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def pause: F[CommandResult] =
    giapi.command(Command(SequenceCommand.PAUSE, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def continue: F[CommandResult] =
    giapi.command(Command(SequenceCommand.CONTINUE, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def stop: F[CommandResult] =
    giapi.command(Command(SequenceCommand.STOP, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def abort: F[CommandResult] =
    giapi.command(Command(SequenceCommand.ABORT, Activity.PRESET_START, Configuration.Zero),
                  DefaultCommandTimeout
    )

  def genericApply(configuration: Configuration)(implicit ev: Applicative[F]): F[CommandResult] =
    // Logger[F].info(
    List(
      configuration.startsWith("ghost:cc:cu"),
      configuration.startsWith("ghost:cc:slu"),
      configuration.startsWith("ghost:cc:spe"),
      configuration.startsWith("ghost:dc:ag"),
      configuration.startsWith("ghost:dc:sv"),
      configuration.startsWith("ghost:dc:red"),
      configuration.startsWith("ghost:dc:blue")
    ).filterNot(_.isEmpty)
      .traverse { c =>
        println(s"Apply $c")
        giapi.command(Command(
                        SequenceCommand.APPLY,
                        Activity.PRESET_START,
                        c
                      ),
                      DefaultCommandTimeout
        )
      }
      .map { results =>
        if (results.exists(_.response == Response.ERROR)) {
          results.find(_.response == Response.ERROR).get
        } else if (results.exists(_.response == Response.NOANSWER)) {
          results.find(_.response == Response.NOANSWER).get
        } else results.headOption.getOrElse(CommandResult(Response.NOANSWER))

      }
}

object GiapiClient {
  val DefaultCommandTimeout: FiniteDuration = 90.seconds
}
