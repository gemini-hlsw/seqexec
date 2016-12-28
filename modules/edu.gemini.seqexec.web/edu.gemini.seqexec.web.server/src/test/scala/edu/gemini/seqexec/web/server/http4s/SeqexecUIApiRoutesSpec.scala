package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.seqexec.engine.Event
import edu.gemini.seqexec.model.Model.SeqexecEvent
import edu.gemini.seqexec.model.{ModelBooPicklers, UserLoginRequest}
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server.SeqexecEngine.Settings

import org.http4s._
import org.http4s.headers.`Set-Cookie`
import org.http4s.util.CaseInsensitiveStringSyntax

import scodec.bits.ByteVector
import squants.time._
import boopickle.Default._

import scalaz.stream.Process.emit
import scalaz.stream.async

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit

import org.scalatest.{FlatSpec, Matchers}

class SeqexecUIApiRoutesSpec extends FlatSpec with Matchers with UriFunctions with ModelBooPicklers with CaseInsensitiveStringSyntax {
  val config = AuthenticationConfig(devMode = true, Hours(8), "token", "abc", useSSL = false, LDAPConfig(Nil))
  val engine = SeqexecEngine(Settings("", LocalDate.now(), "", dhsSim = true, tcsSim = true, instSim = true, gcalSim = true))
  val authService = AuthenticationService(config)
  val inq  = async.boundedQueue[Event](10)
  val out  = async.topic[SeqexecEvent]()
  val queues = (inq, out)

  val service = new SeqexecUIApiRoutes(authService, queues, engine).service

  "SeqexecUIApiRoutes" should
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
}
