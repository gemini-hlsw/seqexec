// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

import cats._
import cats.effect._
import cats.syntax.all._
import edu.gemini.aspen.giapi.commands.Activity
import edu.gemini.aspen.giapi.commands.ConfigPath
import edu.gemini.aspen.giapi.commands.DefaultConfiguration
import edu.gemini.aspen.giapi.commands.HandlerResponse
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.giapi.commands.SequenceCommand
import edu.gemini.aspen.giapi.commands.{ Command => GiapiCommand }
import edu.gemini.aspen.giapi.commands.{ Configuration => GiapiConfiguration }
import edu.gemini.aspen.gmp.commands.jms.client.CommandSenderClient
import giapi.client.syntax.giapiconfig._

package commands {

  final case class CommandResult(response: Response)
  final case class CommandResultException(response: Response, message: String)
      extends RuntimeException

  object CommandResultException {

    def timedOut(after: FiniteDuration): CommandResultException =
      CommandResultException(Response.ERROR, s"Timed out response after: $after")
  }

  final case class Configuration(config: Map[ConfigPath, String]) {

    def value(path: String): Option[String] =
      config.get(ConfigPath.configPath(path))

    def contains(path: String): Boolean =
      config.contains(ConfigPath.configPath(path))

    def remove(path: String): Configuration =
      Configuration(config - ConfigPath.configPath(path))

    def toGiapi: GiapiConfiguration =
      new DefaultConfiguration(new java.util.TreeMap(config.asJava))
  }

  object Configuration {
    val Zero: Configuration = Configuration(Map.empty)

    def single[A: GiapiConfig](key: String, value: A): Configuration =
      Configuration(Map(ConfigPath.configPath(key) -> value.configValue))

    implicit val eq: Eq[Configuration] = Eq.by(_.config)

    implicit val monoid: Monoid[Configuration] = new Monoid[Configuration] {
      def empty: Configuration = Zero

      def combine(a: Configuration, b: Configuration): Configuration =
        Configuration(a.config |+| b.config)
    }
  }

  final case class Command(
    sequenceCommand: SequenceCommand,
    activity:        Activity,
    config:          Configuration
  ) {

    def toGiapi: GiapiCommand =
      new GiapiCommand(sequenceCommand, activity, config.toGiapi)
  }

}

package object commands {
  val DataLabelCfg = "DATA_LABEL"

  implicit val responseEq: Eq[Response] = Eq.instance { case (a, b) =>
    a.name === b.name
  }

  implicit val scEq: Eq[SequenceCommand] = Eq.fromUniversalEquals

  /**
   * Send a command over giapi
   * @param commandsClient Client interface to send the command to the client and await the response
   * @param command The actual command sent
   * @param timeout Timeout to await a response, often 2 seconds
   * @tparam F Effect type
   * @return the result of the operation
   */
  def sendCommand[F[_]: Async](
    commandsClient: CommandSenderClient,
    command:        Command,
    timeout:        Duration
  ): F[CommandResult] =
    Async[F].async_ { cb =>
      val hr = commandsClient.sendCommand(
        command.toGiapi,
        (hr: HandlerResponse, _: GiapiCommand) => {
          if (hr.getResponse === Response.ERROR || hr.getResponse === Response.NOANSWER) {
            cb(Left(CommandResultException(hr.getResponse, hr.getMessage)))
          } else {
            cb(Right(CommandResult(hr.getResponse)))
          }
          ()
        },
        timeout.toMillis
      )
      if (hr.getResponse === Response.ERROR || hr.getResponse === Response.NOANSWER) {
        cb(
          Left(
            CommandResultException(hr.getResponse,
                                   if (hr.getResponse === Response.NOANSWER)
                                     "No answer from the instrument"
                                   else hr.getMessage
            )
          )
        )
      } else if (hr.getResponse === Response.COMPLETED) {
        cb(Right(CommandResult(hr.getResponse)))
      }
    // A third case is ACCEPTED but that is handled on the callback
    }
}
