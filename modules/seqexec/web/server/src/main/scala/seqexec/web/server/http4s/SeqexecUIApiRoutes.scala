// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import java.util.UUID

import cats.data.NonEmptyList
import cats.effect.{ Concurrent, IO, Timer }
import cats.implicits._
import fs2.concurrent.Topic
import fs2.Pipe
import fs2.Stream
import giapi.client.GiapiStatusDb
import giapi.client.StatusValue
import gem.enum.GiapiStatus
import gem.enum.Site
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.middleware.GZip
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{ Binary, Ping }
import org.http4s.headers.`WWW-Authenticate`
import org.log4s._
import scala.concurrent.duration._
import scala.math._
import scodec.bits.ByteVector
import seqexec.model.ClientId
import seqexec.model._
import seqexec.model.events._
import seqexec.server.tcs.GuideConfigDb
import seqexec.web.model.boopickle._
import seqexec.model.config._
import seqexec.web.server.http4s.encoder._
import seqexec.web.server.security.AuthenticationService.AuthResult
import seqexec.web.server.security.AuthenticationService
import seqexec.web.server.security.Http4sAuthentication
import seqexec.web.server.security.TokenRefresher
import seqexec.web.server.OcsBuildInfo
import seqexec.web.common.LogMessage

/**
  * Rest Endpoints under the /api route
  */
class SeqexecUIApiRoutes(site: Site,
                         mode: Mode,
                         auth: AuthenticationService,
                         guideConfigS: GuideConfigDb[IO],
                         giapiDB: GiapiStatusDb[IO],
                         engineOutput: Topic[IO, SeqexecEvent])(
  implicit cio: Concurrent[IO],
           tio: Timer[IO]
) extends BooEncoders with ModelLenses {

  // Logger for client messages
  private val clientLog = getLogger

  private val unauthorized =
    Unauthorized(
      `WWW-Authenticate`(NonEmptyList.of(Challenge("jwt", "seqexec"))))

  // Handles authentication
  private val httpAuthentication = new Http4sAuthentication(auth)

  // Stream of updates to the guide config db
  val guideConfigEvents =
    guideConfigS.discrete
      .map(g => GuideConfigUpdate(g.tcsGuide))
      .map(toFrame)

  // Stream of updates to gpi align an calib process
  // This is fairly custom for one use case and we'd rather have a more
  // generalized mechanism.
  // Also we may want to send this through another websocket but it would
  // complicate the client
  val giapiDBEvents =
    giapiDB.discrete
      .map(_.get(GiapiStatus.GpiAlignAndCalibState.statusItem).flatMap(StatusValue.intValue))
      .collect {
        case Some(x) => AlignAndCalibEvent(x)
      }
      .map(toFrame)

  /**
    * Creates a process that sends a ping every second to keep the connection alive
    */
  private def pingStream: Stream[IO, Ping] =
    Stream.fixedRate[IO](1.second).flatMap(_ => Stream.emit(Ping()))

  val publicService: HttpRoutes[IO] = GZip { HttpRoutes.of {

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
            unauthorized
        }
      }

      case POST -> Root / "seqexec" / "logout"              =>
        // Clean the auth cookie
        val cookie = ResponseCookie(auth.config.cookieName, "", path = "/".some,
          secure = auth.config.useSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").map(_.removeCookie(cookie))

    }}

  val protectedServices: AuthedRoutes[AuthResult, IO] =
    AuthedRoutes.of {
      // Route used for testing only
      case GET  -> Root  / "log" / count as _ if mode === Mode.Development =>
        for {_ <- 0 until min(1000, max(0, count.toInt))} {
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

      case auth @ POST -> Root / "seqexec" / "site" as user =>
        val userName = user.fold(_ => "Anonymous", _.displayName)
        // Login start
        IO(clientLog.info(
          s"$userName connected from ${auth.req.remoteHost.getOrElse("Unknown")}")) *>
          Ok(s"$site")

      case GET -> Root / "seqexec" / "events" as user        =>
        // If the user didn't login, anonymize
        val anonymizeF: SeqexecEvent => SeqexecEvent = user.fold(_ => anonymize _, _ => identity _)

        def initialEvent(clientId: ClientId): Stream[IO, WebSocketFrame] =
          Stream.emit(toFrame(ConnectionOpenEvent(user.toOption, clientId, OcsBuildInfo.version)))

        def engineEvents(clientId: ClientId): Stream[IO, WebSocketFrame]  =
          engineOutput
            .subscribe(1)
            .map(anonymizeF)
            .filter(filterOutNull)
            .filter(filterOutOnClientId(clientId))
            .map(toFrame)

        // We don't care about messages sent over ws by clients
        val clientEventsSink: Pipe[IO, WebSocketFrame, Unit] = _.evalMap(_ => IO.unit)

        // Create a client specific websocket
        for {
          clientId <- IO.apply(ClientId(UUID.randomUUID()))
          initial  = initialEvent(clientId)
          streams  = Stream(pingStream, guideConfigEvents, giapiDBEvents, engineEvents(clientId)).parJoinUnbounded
          ws       <- WebSocketBuilder[IO].build(initial ++ streams, clientEventsSink)
        } yield ws

    }

  def service: HttpRoutes[IO] = publicService <+> TokenRefresher(GZip(httpAuthentication.optAuth(protectedServices)), httpAuthentication)

  // Event to WebSocket frame
  private def toFrame(e: SeqexecEvent) =
    Binary(ByteVector(trimmedArray(e)))

  // Stream seqexec events to clients and a ping
  private def anonymize(e: SeqexecEvent) =
    // Hide the name and target name for anonymous users
    (telescopeTargetNameT.set("*****") andThen observeTargetNameT.set("*****") andThen sequenceNameT.set(""))(e)

  // Filter out NullEvents from the engine
  private def filterOutNull = (e: SeqexecEvent) => e match {
    case NullEvent => false
    case _         => true
  }

  // Messages with a clientId are only sent to the matching cliend
  private def filterOutOnClientId(clientId: ClientId) = (e: SeqexecEvent) => e match {
    case e: ForClient if e.clientId =!= clientId => false
    case _                                       => true
  }

}
