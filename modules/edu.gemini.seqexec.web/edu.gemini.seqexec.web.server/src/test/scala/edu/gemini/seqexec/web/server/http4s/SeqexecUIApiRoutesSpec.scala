package edu.gemini.seqexec.web.server.http4s

import java.nio.ByteBuffer

import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.seqexec.engine.Event
import edu.gemini.seqexec.model.Model.SeqexecEvent
import edu.gemini.seqexec.model.{ModelBooPicklers, UserDetails, UserLoginRequest}
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server.SeqexecEngine.Settings
import org.http4s._
import org.http4s.headers.{`Set-Cookie`, Cookie => CookieHeader}
import org.http4s.util.CaseInsensitiveStringSyntax
import org.http4s.util.NonEmptyList._
import scodec.bits.ByteVector
import squants.time._
import boopickle.Default._

import scalaz.stream.Process.emit
import scalaz.stream.Process
import scalaz.stream.async
import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit

import edu.gemini.seqexec.model.Model.SeqexecEvent.ConnectionOpenEvent
import org.http4s.websocket.WebsocketBits
import org.scalatest.{FlatSpec, Matchers}

import scalaz.OptionT
import scalaz.concurrent.Task
import scalaz.stream.async.mutable.{Queue, Topic}

class SeqexecUIApiRoutesSpec extends FlatSpec with Matchers with UriFunctions with ModelBooPicklers with CaseInsensitiveStringSyntax {
  val config = AuthenticationConfig(devMode = true, Hours(8), "token", "abc", useSSL = false, LDAPConfig(Nil))
  val engine = SeqexecEngine(Settings("", LocalDate.now(), "", dhsSim = true, tcsSim = true, instSim = true, gcalSim = true))
  val authService = AuthenticationService(config)
  val inq: Queue[Event] = async.boundedQueue[Event](10)
  val out: Topic[SeqexecEvent] = async.topic[SeqexecEvent]()
  val queues: (Queue[Event], Topic[SeqexecEvent]) = (inq, out)

  val service: Service[Request, Response] = new SeqexecUIApiRoutes(authService, queues, engine).service

  "SeqexecUIApiRoutes login" should
    "reject requests without body" in {
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"))).unsafePerformSync.status should equal(Status.BadRequest)
    }
    it should "reject GET requests" in {
      // This should in principle return a 405
      // see https://github.com/http4s/http4s/issues/234
      service.apply(Request(uri = uri("/seqexec/login"))).unsafePerformSync.status should equal(Status.NotFound)
    }
    it should "reject requests with string body" in {
      val b = emit(ByteVector.view("hello".getBytes(StandardCharsets.UTF_8)))
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync.status should equal(Status.BadRequest)
    }
    it should "not authorize requests with unmatching credentials" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("a", "b"))))
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync.status should equal(Status.Unauthorized)
    }
    it should "authorize requests with matching credentials and return a Cookie" in {
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      val response: Response = service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafePerformSync
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
      cookieOpt.flatMap(_.cookie.expires).map(i => i.isAfter(minExp) && i.isBefore(maxExp)) shouldBe Some(true)
    }

  "SeqexecUIApiRoutes logout" should
    "reject GET requests" in {
      // This should in principle return a 405
      // see https://github.com/http4s/http4s/issues/234
      service.apply(Request(uri = uri("/seqexec/logout"))).unsafePerformSync.status should equal(Status.NotFound)
    }
    it should "reject unauthorized requests" in {
      service.apply(Request(method = Method.POST, uri = uri("/seqexec/logout"))).unsafePerformSync.status should equal(Status.Unauthorized)
    }
    it should "remove the cookie on logout" in {
      // First make a valid cookie
      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
      for {
        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
        cookieHeader = loginResp.headers.find(_.name === "Set-Cookie".ci)
        setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
        authCookie   = new CookieHeader(nels(setCookie.cookie))
        logoutResp   <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/logout")).putHeaders(authCookie)).map(Option.apply))
      } yield logoutResp
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
          events     <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*)).map(Option.apply))
          exchange   <- OptionT(Task.now(events.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
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
          cookieHeader =  loginResp.headers.find(_.name === "Set-Cookie".ci)
          setCookie    <- OptionT(Task.now(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
          authCookie   =  new CookieHeader(nels(setCookie.cookie))
          events       <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*).putHeaders(authCookie)).map(Option.apply))
          exchange     <- OptionT(Task.now(events.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
          frames       <- OptionT(exchange.run(Process.empty).take(1).runLog.map(Option.apply))
          firstFrame   <- OptionT(Task.now(frames.headOption.collect {case WebsocketBits.Binary(data, _) => data}))
          firstEvent   <- OptionT(Task.now(Unpickle[SeqexecEvent].fromBytes(ByteBuffer.wrap(firstFrame))).map(Option.apply))
        } yield firstEvent

      openEvent.run.unsafePerformSync shouldBe Some(ConnectionOpenEvent(Some(UserDetails("telops", "Telops"))))
    }

}
