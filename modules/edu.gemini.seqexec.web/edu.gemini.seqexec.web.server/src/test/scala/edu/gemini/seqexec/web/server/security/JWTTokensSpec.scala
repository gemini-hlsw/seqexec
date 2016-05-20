package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.web.common.UserDetails
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class JWTTokensSpec extends FlatSpec with Matchers with PropertyChecks {
  "JWT Tokens" should "encode/decode" in {
    forAll { (u: String, p: String) =>
      val userDetails = UserDetails(u, p)
      val token = AuthenticationService.buildToken(userDetails)
      Success(userDetails) shouldEqual AuthenticationService.decodeToken(token)
    }
  }
}
