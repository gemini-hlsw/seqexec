// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.{ ContextShift, IO, Timer }
import seqexec.model.events._
import seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import fs2.concurrent.Topic
import fs2.Stream
import org.http4s._
import org.http4s.syntax.StringSyntax
import org.http4s.Uri.uri
import org.scalatest.{FlatSpec, Matchers, NonImplicitAssertions}
import squants.time._
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals", "org.wartremover.warts.OptionPartial"))
class SeqexecUIApiRoutesSpec extends FlatSpec with Matchers with StringSyntax with NonImplicitAssertions {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  private val config = AuthenticationConfig(devMode = true, Hours(8), "token", "abc", useSSL = false, LDAPConfig(Nil))
  private val authService = AuthenticationService(config)
  val out: Stream[IO, Topic[IO, SeqexecEvent]] = Stream.eval(Topic[IO, SeqexecEvent](NullEvent))

  private val service =
    for {
      o <- out
    } yield new SeqexecUIApiRoutes("GS", true, authService, o).service

  "SeqexecUIApiRoutes login" should
    "reject requests without body" in {
      for {
        s <- service
      } yield {
        s.apply(Request(method = Method.POST, uri = uri("/seqexec/login"))).value.unsafeRunSync.map(_.status) should contain(Status.FailedDependency)
      }
    }
    it should "reject GET requests" in {
      // This should in principle return a 405
      // see https://github.com/http4s/http4s/issues/234
      for {
        s <- service
      } yield s.apply(Request(uri = uri("/seqexec/login"))).value.unsafeRunSync.map(_.status) should contain(Status.NotFound)
    }
//    it should "reject requests with string body" in {
//      val b = emit(ByteVector.view("hello".getBytes(StandardCharsets.UTF_8)))
//      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafeRunSync.orNotFound.status should equal(Status.BadRequest)
//    }
//    it should "not authorize requests with unmatching credentials" in {
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("a", "b"))))
//      service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafeRunSync.orNotFound.status should equal(Status.Unauthorized)
//    }
//    it should "authorize requests with matching credentials and return a Cookie" in {
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//      val response: Response = service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).unsafeRunSync.orNotFound
//      response.status should equal(Status.Ok)
//      atLeast (1, response.headers.toList.map(_.name)) should be ("Set-Cookie".ci)
//      val cookieHeader = response.headers.find(_.name === "Set-Cookie".ci)
//      val cookieOpt = cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)
//      cookieOpt.map(_.cookie.httpOnly) shouldBe Some(true)
//      cookieOpt.map(_.cookie.secure) shouldBe Some(false)
//      cookieOpt.map(_.cookie.name) shouldBe Some("token")
//      // We cannot be that precise but let's assume expiration is further than 7 hours and less than 9 hours into the future
//      val minExp = Instant.now().plus(7, ChronoUnit.HOURS)
//      val maxExp = Instant.now().plus(9, ChronoUnit.HOURS)
//      cookieOpt.flatMap(_.cookie.expires).map(i => i.toInstant.isAfter(minExp) && i.toInstant.isBefore(maxExp)) shouldBe Some(true)
//    }
//
//  "SeqexecUIApiRoutes logout" should
//    "reject GET requests" in {
//      // This should in principle return a 405
//      // see https://github.com/http4s/http4s/issues/234
//      service.apply(Request(uri = uri("/seqexec/logout"))).unsafeRunSync.orNotFound.status should equal(Status.NotFound)
//    }
//    it should "remove the cookie on logout" in {
//      // First make a valid cookie
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//      val logout = for {
//        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
//        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//        setCookie    <- OptionT(IO.pure(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//        logoutResp   <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/logout")).addCookie(setCookie.cookie)).map(Option.apply))
//      } yield logoutResp.orNotFound
//      val cookieOpt = logout.run.unsafeRunSync.flatMap(_.headers.find(_.name === "Set-Cookie".ci).flatMap(u => `Set-Cookie`.parse(u.value).toOption))
//      // On logout we clear the cookie
//      cookieOpt.map(_.cookie.content) shouldBe Some("")
//    }
//
//  "SeqexecUIApiRoutes sequences" should
//    "reject GET requests" in {
//      service.apply(Request(uri = uri("/seqexec/sequence/GS-2017A-Q-0-1"))).unsafeRunSync.orNotFound.status should equal(Status.Unauthorized)
//    }
//    it should "reject requests without authentication" in {
//      service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/GS-2017A-Q-0-1"))).unsafeRunSync.orNotFound.status should equal(Status.Unauthorized)
//    }
//    it should "accept requests with a valid cookie though the sequence is not found" in {
//      // First make a valid cookie
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//      val sequence = for {
//        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
//        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//        setCookie    <- OptionT(IO.pure(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//        seqResp      <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/GS-2016A-Q-0-1999999")).addCookie(setCookie.cookie)).map(Option.apply))
//      } yield seqResp
//      sequence.run.unsafeRunSync.flatMap(_.toOption).map(_.status) shouldBe Some(Status.NotFound)
//    }
//    it should "reject requests with non valid sequence ids" in {
//      // First make a valid cookie
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//      val sequence = for {
//        loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
//        cookieHeader = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//        setCookie    <- OptionT(IO.pure(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//        seqResp      <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/abc")).addCookie(setCookie.cookie)).map(Option.apply))
//      } yield seqResp
//      sequence.run.unsafeRunSync.flatMap(_.toOption).map(_.status) shouldBe Some(Status.BadRequest)
//    }
//    it should "replace the authentication cookie" in {
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//      val sequence = for {
//        loginResp           <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
//        cookieHeader        = loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//        setCookie           <- OptionT(IO.pure(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//        _                   <- OptionT(IO.schedule(().some, 1.seconds)) // We need to add a delay to have a different cookie
//        seqResp             <- OptionT(service.apply(Request(method = Method.GET, uri = uri("/seqexec/sequence/abc")).addCookie(setCookie.cookie)).map(Option.apply))
//        updatedCookieHeader = seqResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//        updatedCookie       <- OptionT(IO.pure(updatedCookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//      } yield setCookie.cookie.content =/= updatedCookie.cookie.content
//      sequence.run.unsafeRunSync shouldBe Some(true)
//    }
//
//  val handshakeHeaders: List[Header] = List(
//    Header("Upgrade", "websocket"),
//    Header("Connection", "Upgrade"),
//    Header("Sec-WebSocket-Key", "GIBYEYWBkPl1qZfZQydmHw==htpp4"),
//    Header("Sec-WebSocket-Version", "13"),
//    Header("Sec-WebSocket-Extensions", "permessage-deflate; client_max_window_bits"),
//    Header("Origin", "http://localhost"))
//
//  "SeqexecUIApiRoutes events" should
//    "return no user if logged anonymously" in {
//      val openEvent =
//        for {
//          response   <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*)).map(Option.apply))
//          exchange   <- OptionT(IO.pure(response.orNotFound.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
//          frames     <- OptionT(exchange.run(Process.empty).take(1).runLog.map(Option.apply))
//          firstFrame <- OptionT(IO.pure(frames.headOption.collect {case WebsocketBits.Binary(data, _) => data}))
//          firstEvent <- OptionT(IO.pure(Unpickle[SeqexecEvent].fromBytes(ByteBuffer.wrap(firstFrame))).map(Option.apply))
//        } yield firstEvent
//
//      openEvent.run.unsafeRunSync should matchPattern {
//        case Some(ConnectionOpenEvent(None, _)) =>
//      }
//    }
//    it should "return the user if the cookie is provided" in {
//      val b = emit(ByteVector.view(Pickle.intoBytes(UserLoginRequest("telops", "pwd"))))
//
//      val openEvent =
//        for {
//          loginResp    <- OptionT(service.apply(Request(method = Method.POST, uri = uri("/seqexec/login"), body = b)).map(Option.apply))
//          cookieHeader =  loginResp.orNotFound.headers.find(_.name === "Set-Cookie".ci)
//          setCookie    <- OptionT(IO.pure(cookieHeader.flatMap(u => `Set-Cookie`.parse(u.value).toOption)))
//          response     <- OptionT(service.apply(Request(uri = uri("/seqexec/events"), method = Method.GET).putHeaders(handshakeHeaders: _*).addCookie(setCookie.cookie)).map(Option.apply))
//          exchange     <- OptionT(IO.pure(response.orNotFound.attributes.get(org.http4s.server.websocket.websocketKey).map(_.exchange)))
//          frames       <- OptionT(exchange.run(Process.empty).take(1).runLog.map(Option.apply))
//          firstFrame   <- OptionT(IO.pure(frames.headOption.collect {case WebsocketBits.Binary(data, _) => data}))
//          firstEvent   <- OptionT(IO.pure(Unpickle[SeqexecEvent].fromBytes(ByteBuffer.wrap(firstFrame))).map(Option.apply))
//        } yield firstEvent
//
//      openEvent.run.unsafeRunSync should matchPattern {
//        case Some(ConnectionOpenEvent(Some(UserDetails("telops", "Telops")), _)) =>
//      }
//    }

}
