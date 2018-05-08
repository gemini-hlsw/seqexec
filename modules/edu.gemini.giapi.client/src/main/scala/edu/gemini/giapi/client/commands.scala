// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats.Eq
import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.giapi.commands.{Command, HandlerResponse}
import edu.gemini.aspen.gmp.commands.jms.client.CommandSenderClient
import fs2.async
import scala.concurrent.ExecutionContext

package commands {
  sealed trait CommandResult {
    def isError: Boolean = false
  }
  final case class Completed(response: Response) extends CommandResult
  final case class Error(response: Response, message: String) extends CommandResult {
    override def isError = true
  }
  final case class Accepted[F[_]](commandName: Option[String],
                                  response: Response,
                                  completion: async.Promise[F, CommandResult])
      extends CommandResult
  final case class CommandFailure(e: Throwable) extends CommandResult
}

package object commands {

  type CommandOperationResult[F[_]] = F[CommandResult]

  implicit val responseEq: Eq[Response] = Eq.instance {
    case (a, b) => a.name === b.name
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
  def sendCommand[F[_]: Effect](
      commandsClient: CommandSenderClient,
      command: Command,
      commandName: Option[String])(implicit ec: ExecutionContext): F[CommandResult] =
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
      } else if (hr.getResponse === Response.COMPLETED || hr.getResponse === Response.ACCEPTED) {
        cb(Right(Completed(hr.getResponse)))
      } else {
        cb(Right(Accepted(commandName, hr.getResponse, null)))
      }
    }
}
