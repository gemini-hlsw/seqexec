// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit

import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.seqexec.model.events.SeqexecEvent
import edu.gemini.seqexec.model.events.SeqexecEvent.ConnectionOpenEvent
import edu.gemini.seqexec.model.{ModelBooPicklers, UserDetails, UserLoginRequest}
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server.executeEngine
import org.http4s._
import org.http4s.headers.`Set-Cookie`
import org.http4s.syntax.StringSyntax
import org.http4s.websocket.WebsocketBits
import scodec.bits.ByteVector
import squants.time._
import boopickle.Default._

import scalaz.stream.Process.emit
import scalaz.stream.Process
import scalaz.stream.async
import scalaz.OptionT
import scalaz.syntax.std.option._
import scalaz.syntax.equal._
import scalaz.std.AllInstances._
import scalaz.concurrent.Task
import scalaz.stream.async.mutable.{Queue, Topic}
import org.scalatest.{FlatSpec, Matchers, NonImplicitAssertions}

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals", "org.wartremover.warts.OptionPartial"))
class SeqexecUIApiRoutesSpec extends FlatSpec with Matchers with UriFunctions with StringSyntax with NonImplicitAssertions {
  import ModelBooPicklers._

  private val config = AuthenticationConfig(devMode = true, Hours(8), "token", "abc", useSSL = false, LDAPConfig(Nil))
  private val engine = SeqexecEngine(SeqexecEngine.defaultSettings.copy(date = LocalDate.now))
  private val authService = AuthenticationService(config)
  val inq: Queue[executeEngine.EventType] = async.boundedQueue[executeEngine.EventType](10)
  val out: Topic[SeqexecEvent] = async.topic[SeqexecEvent]()
  val queues: (Queue[executeEngine.EventType], Topic[SeqexecEvent]) = (inq, out)

  val service: Service[Request, MaybeResponse] = new SeqexecUIApiRoutes(true, authService, queues, engine).service

  "SeqexecUIApiRoutes login" should
    "reject requests without body" in {
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"))).unsafePerformSync.orNotFound.status should equal(Status.BadRequest)
    }
    it should "reject GET requests" in {
      // This should in principle return a 405
      // see https://github.com/http4s/http4s/issues/234
      service.apply(Request(uri = uri("/seqexec/login"))).unsafePerformSync.orNotFound.status should equal(Status.NotFound)
    }
    it should "reject requests with string body" in {
      val b = emit(ByteVector.view("hello".getBytes(StandardCharsets.UTF_8)))
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync.orNotFound.status should equal(Status.BadRequest)
    }
    it should "not authorize requests with unmatching credentials" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("a", "b"))))
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync.orNotFound.status should equal(Status.Unauthorized)
    }
    it should "authorize requests with matching credentials and return a Cookie" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val response: Response = service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync.orNotFound
      response.status should equal(Status.Ok)
      atLeast (1, response.headers.toList.map(_.name)) should be ("Set-Cookie".ci)
      val cookieHeader = response.headers.find(_.name === "Set-Cookie".ci)
      val cookieOpt = cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)
      cookieOpt.map(_.cookie.httpOnly) shouldBe Some(true)
      cookieOpt.map(_.cookie.secure) shouldBe Some(false)
      cookieOpt.map(_.cookie.name) shouldBe Some("token")
      // We cannot be that precise but let's assume expiration is further than 7 hours and less than 9 hours into the future
      val minExp = Instant.now().plus(7, ChronoUnit.HOURS)
      val maxExp = Instant.now().plus(9, ChronoUnit.HOURS)
      cookieOpt.flatMap(_.cookie.expires).map(i => i.toInstant.isAfter(minExp) && i.toInstant.isBefore(maxExp)) shouldBe Some(true)
    }

  "SeqexecUIApiRoutes logout" should
    "reject GET requests" in {
      // This should in principle return a 405
      // see https://github.com/http4s/http4s/issues/234
      service.apply(Request(uri = uri("/seqexec/logout"))).unsafePerformSync.orNotFound.status should equal(Status.NotFound)
    }
    it should "remove the cookie on logout" in {
      // First make a valid cookie
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val logout = for {
        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
        setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
        logoutResp   <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/logout")).addCookie(setCookie.cookie)).map(Option.apply))
      } yield logoutResp.orNotFound
      val cookieOpt = logout.run.unsafePerformSync.flatMap(_.headers.find(_.name === "Set-Cookie".ci).flatMap(u => `Set-Cookie`.parse(u.value).toOption))
      // On logout we clear the cookie
      cookieOpt.map(_.cookie.content) shouldBe Some("")
    }

  "SeqexecUIApiRoutes sequences" should
    "reject GET requests" in {
      service.apply(Request(uri = uri("/seqexec/sequence/GS-2017A-Q-0-1"))).unsafePerformSync.orNotFound.status should equal(Status.Unauthorized)
    }
    it should "reject requests without authentication" in {
      service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/GS-2017A-Q-0-1"))).unsafePerformSync.orNotFound.status should equal(Status.Unauthorized)
    }
    it should "accept requests with a valid cookie though the sequence is not found" in {
      // First make a valid cookie
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val sequence = for {
        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
        setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
        seqResp      <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/GS-2016A-Q-0-1999999")).addCookie(setCookie.cookie)).map(Option.apply))
      } yield seqResp
      sequence.run.unsafePerformSync.flatMap(_.toOption).map(_.status) shouldBe Some(Status.NotFound)
    }
    it should "reject requests with non valid sequence ids" in {
      // First make a valid cookie
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val sequence = for {
        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
        setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
        seqResp      <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/abc")).addCookie(setCookie.cookie)).map(Option.apply))
      } yield seqResp
      sequence.run.unsafePerformSync.flatMap(_.toOption).map(_.status) shouldBe Some(Status.BadRequest)
    }
    it should "replace the authentication cookie" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val sequence = for {
        loginResp           <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
        cookieHeader        = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
        setCookie           <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
        _                   <- OptionT(Task.schedule(().some, 1.seconds)) // We need to add a delay to have a different cookie
        seqResp             <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/abc")).addCookie(setCookie.cookie)).map(Option.apply))
        updatedCookieHeader = seqResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
        updatedCookie       <- OptionT(Task.now(updatedCookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
      } yield setCookie.cookie.content =/= updatedCookie.cookie.content
      sequence.run.unsafePerformSync shouldBe Some(true)
    }

  val handshakeHeaders: List[Header] = List(
    Header("Upgrade", "websocket"),
    Header("Connection", "Upgrade"),
    Header("Sec-WebSocket-Key", "GIBYEYWBkPl1qZfZQydmHw==htpp4"),
    Header("Sec-WebSocket-Version", "13"),
    Header("Sec-WebSocket-Extensions", "permessage-deflate; client_max_window_bits"),
    Header("Origin", "http://localhost"))

  "SeqexecUIApiRoutes events" should
    "return no user if logged anonymously" in {
      val openEvent =
        for {
          response   <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*)).map(Option.apply))
          exchange   <- OptionT(Task.now(response.orNotFound.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
          frames     <- OptionT(exchange.run(Process.empty).take(1).runLog.map(Option.apply))
          firstFrame <- OptionT(Task.now(frames.headOption.collect {case WebsocketBits.Binary(data, _) => data}))
          firstEvent <- OptionT(Task.now(Unpickle[SeqexecEvent].fromBytes(ByteBuffer.wrap(firstFrame))).map(Option.apply))
        } yield firstEvent

      openEvent.run.unsafePerformSync shouldBe Some(ConnectionOpenEvent(None))
    }
    it should "return the user if the cookie is provided" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))

      val openEvent =
        for {
          loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
          cookieHeader =  loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
          setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
          response     <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*).addCookie(setCookie.cookie)).map(Option.apply))
          exchange     <- OptionT(Task.now(response.orNotFound.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
          frames       <- OptionT(exchange.run(Process.empty).take(1).runLog.map(Option.apply))
          firstFrame   <- OptionT(Task.now(frames.headOption.collect {case WebsocketBits.Binary(data, _) => data}))
          firstEvent   <- OptionT(Task.now(Unpickle[SeqexecEvent].fromBytes(ByteBuffer.wrap(firstFrame))).map(Option.apply))
        } yield firstEvent

      openEvent.run.unsafePerformSync shouldBe Some(ConnectionOpenEvent(Some(UserDetails("telops", "Telops"))))
    }

}
