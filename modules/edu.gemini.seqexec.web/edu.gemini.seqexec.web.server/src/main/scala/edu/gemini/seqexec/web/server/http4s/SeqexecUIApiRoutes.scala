// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s

import java.util.UUID

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.model.Model.{ClientID, Conditions, SequenceId, SequencesQueue}
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.model.ModelBooPicklers.trimmedArray
import edu.gemini.seqexec.model.events._
import edu.gemini.seqexec.server
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import edu.gemini.seqexec.web.server.security.{AuthenticationService, Http4sAuthentication, TokenRefresher}
import edu.gemini.spModel.core.SPBadIDException
import edu.gemini.web.common.LogMessage
import fs2.async.mutable.Topic
import fs2.{Scheduler, Sink, Stream}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.middleware.GZip
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebsocketBits._
import org.http4s.headers.`WWW-Authenticate`
import org.log4s._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.math._

/**
  * Rest Endpoints under the /api route
  */
class SeqexecUIApiRoutes(devMode: Boolean, auth: AuthenticationService, events: (server.EventQueue, Topic[IO, SeqexecEvent]), se: SeqexecEngine) extends BooEncoders with ModelLenses {

  // Logger for client messages
  private val clientLog = getLogger

  // Handles authentication
  private val httpAuthentication = new Http4sAuthentication(auth)

  private val (inputQueue, engineOutput) = events

  /**
    * Creates a process that sends a ping every second to keep the connection alive
    */
  private def pingStream: Stream[IO, Ping] =
    Scheduler[IO](corePoolSize = 1).flatMap { scheduler =>
      scheduler.fixedRate[IO](1.second).flatMap(_ => Stream.emit(Ping()))
    }

  val publicService: HttpService[IO] = GZip { HttpService {

    case req @ POST -> Root / "seqexec" / "login" =>
      req.decode[UserLoginRequest] { (u: UserLoginRequest) =>
        // Try to authenticate
        auth.authenticateUser(u.username, u.password).flatMap {
          case Right(user) =>
            // Log who logged in
            // Note that the call to read a remoteAddr may do a DNS lookup
            IO(clientLog.info(s"${user.displayName} logged in from ${req.remoteHost.getOrElse("Unknown")}")) *>
            // if successful set a cookie
            httpAuthentication.loginCookie(user) >>= { cookie => Ok(user).map(_.addCookie(cookie)) }
          case Left(_) =>
            Unauthorized(`WWW-Authenticate`(NonEmptyList.of(Challenge("jwt", "seqexec"))))
        }
      }

      case POST -> Root / "seqexec" / "logout"              =>
        // Clean the auth cookie
        val cookie = Cookie(auth.config.cookieName, "", path = "/".some,
          secure = auth.config.useSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").map(_.removeCookie(cookie))

    }}

  val protectedServices: AuthedService[AuthResult, IO] =
    AuthedService {
      // Route used for testing only
      case GET  -> Root  / "log" / count as _ if devMode =>
        for {i <- 0 until min(1000, max(0, count.toInt))} {
          clientLog.info("info")
          clientLog.warn("warn")
          clientLog.error("error")
        }
        Ok("")

      case auth @ POST -> Root / "seqexec" / "log" as user =>
        auth.req.decode[LogMessage] { msg =>
          val userName = user.fold(_ => "Anonymous", _.displayName)
          // Always return ok
          // Use remoteAddr to avoid an expensive DNS lookup
          IO(clientLog.info(s"$userName on ${auth.req.remoteAddr.getOrElse("Unknown")}: ${msg.msg}")) *> Ok("")
        }

      case auth @ POST -> Root / "seqexec" / "start" as user =>
        val userName = user.fold(_ => "Anonymous", _.displayName)
        // Always return ok
        IO(clientLog.info(s"$userName connected from ${auth.req.remoteHost.getOrElse("Unknown")}")) *> Ok("")

      case GET -> Root / "seqexec" / "events" as user        =>
        // Stream seqexec events to clients and a ping
        def anonymize(e: SeqexecEvent) = {
            // Hide the name and target name for anonymous users
            (telescopeTargetNameT.set("*****") andThen observeTargetNameT.set("*****") andThen sequenceNameT.set(""))(e)
        }
        def filterOutNull = (e: SeqexecEvent) => e match {
          case NullEvent => false
          case _         => true
        }
        def filterOutOnClientId(clientId: ClientID) = (e: SeqexecEvent) => e match {
          case e: ForClient if e.clientId =!= clientId => false
          case _                                       => true
        }
        // If the user didn't login, anonymize
        val anonymizeF: SeqexecEvent => SeqexecEvent = user.fold(_ => anonymize _, _ => identity _)
        // Create a client specific process
        for {
          clientId <- IO.apply(UUID.randomUUID())
          ws       <-
            WebSocketBuilder[IO].build(Stream.emit(Binary(trimmedArray(ConnectionOpenEvent(user.toOption, clientId)))) ++
              (pingStream merge engineOutput.subscribe(1).map(anonymizeF).filter(filterOutNull).filter(filterOutOnClientId(clientId)).map(v => Binary(trimmedArray(v)))), Sink(_ => IO.unit))
        } yield ws

      case GET -> Root / "seqexec" / "sequence" / oid as user =>
        user.toOption.fold(Unauthorized(`WWW-Authenticate`(NonEmptyList.of(Challenge("jwt", "seqexec"))))) { _ =>
          for {
            obsId <- IO.fromEither(Either.catchNonFatal(new SPObservationID(oid)))
            u     <- se.load(inputQueue, obsId)
            resp  <- u.fold(_ => NotFound(s"Not found sequence $oid"), _ =>
              Ok(SequencesQueue[SequenceId](Conditions.default, None, List(oid))))
          } yield resp
        }.attempt.flatMap {
          case Left(_: SPBadIDException) => BadRequest(s"Bad sequence id $oid")
          case Left(r) => IO.raiseError(r)
          case Right(r) => IO.pure(r)
        }
    }

  def service: HttpService[IO] = publicService <+> TokenRefresher(GZip(httpAuthentication.optAuth(protectedServices)), httpAuthentication)
}
