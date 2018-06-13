// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Eq
import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.giapi.commands.{Command, HandlerResponse}
import edu.gemini.aspen.gmp.commands.jms.client.CommandSenderClient

import scala.concurrent.duration.Duration

package commands {
  sealed trait CommandResult
  final case class Completed(response: Response)            extends CommandResult
  final case class Error[A](response: Response, message: A) extends CommandResult
}

package object commands {
  val DataLabelCfg = "DATA_LABEL"

  implicit class CommandResultOps(val cr: CommandResult) extends AnyVal {

    def isError: Boolean = cr match {
      case Error(_, _) => true
      case _           => false
    }
  }

  implicit val responseEq: Eq[Response] = Eq.instance {
    case (a, b) => a.name === b.name
  }

  /**
    * Send a command over giapi
    * @param commandsClient Client interface to send the command to the client and await the response
    * @param command The actual command sent
    * @param timeout Timeout to await a response, often 2 seconds
    * @tparam F Effect type
    * @return the result of the operation
    */
  def sendCommand[F[_]: Async](commandsClient: CommandSenderClient,
                               command: Command,
                               timeout: Duration): F[CommandResult] =
    Async[F].async { cb =>
      val hr = commandsClient.sendCommand(
        command,
        (hr: HandlerResponse, _: Command) => {
          if (hr.getResponse === Response.ERROR || hr.getResponse === Response.NOANSWER) {
            cb(Right(Error(hr.getResponse, hr.getMessage)))
          } else {
            cb(Right(Completed(hr.getResponse)))
          }
          ()
        },
        timeout.toMillis
      )
      if (hr.getResponse === Response.ERROR || hr.getResponse === Response.NOANSWER) {
        cb(
          Right(
            Error(hr.getResponse,
                  if (hr.getResponse === Response.NOANSWER) "No answer from the instrument"
                  else hr.getMessage)))
      } else if (hr.getResponse === Response.COMPLETED) {
        cb(Right(Completed(hr.getResponse)))
      }
    // A third case is ACCEPTED but that is handled on the callback
    }
}
