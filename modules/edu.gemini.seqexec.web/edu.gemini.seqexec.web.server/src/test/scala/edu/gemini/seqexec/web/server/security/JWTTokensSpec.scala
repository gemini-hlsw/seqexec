package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.model.UserDetails
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success
import scalaz.\/-

class JWTTokensSpec extends FlatSpec with Matchers with PropertyChecks {
  val ldapConfig = LDAPConfig(Nil, Nil)
  val config = AuthenticationConfig(devMode = true, 8, "token", "key", useSSL = false, ldapConfig)
  val authService = AuthenticationService(config)

  "JWT Tokens" should "encode/decode" in {
    forAll { (u: String, p: String) =>
      val userDetails = UserDetails(u, p)
      val token = authService.buildToken(userDetails)
      \/-(userDetails) shouldEqual authService.decodeToken(token)
    }
  }
}
