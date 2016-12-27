package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.seqexec.engine.Event
import edu.gemini.seqexec.model.Model.SeqexecEvent
import edu.gemini.seqexec.model.{ModelBooPicklers, UserLoginRequest}
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server.SeqexecEngine.Settings
import org.http4s._
import scodec.bits.ByteVector
import squants.time._
import boopickle.Default._

import scalaz.stream.Process.emit
import scalaz.stream.async
import java.nio.charset.StandardCharsets
import java.time.LocalDate

import org.http4s.util.CaseInsensitiveStringSyntax
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
      println(response.attributes)
      response.headers.toList.exists(_.name == "Set-Cookie".ci) should be (true)
    }
}
