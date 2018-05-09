// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats.Eq
import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.giapi.commands.{Command, HandlerResponse}
import edu.gemini.aspen.gmp.commands.jms.client.CommandSenderClient

package commands {
  sealed trait CommandResult {
    def isError: Boolean = false
  }
  final case class Completed(response: Response) extends CommandResult
  final case class Error(response: Response, message: String) extends CommandResult {
    override def isError = true
  }
}

package object commands {
  val DataLabelCfg = "DATA_LABEL"

  implicit val responseEq: Eq[Response] = Eq.instance {
    case (a, b) => a.name === b.name
  }

  def sendCommand[F[_]: Async](commandsClient: CommandSenderClient,
                               command: Command): F[CommandResult] =
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
        2000
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
