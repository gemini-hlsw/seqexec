package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.web.common.UserDetails
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class FreeLDAPAuthenticationServiceSpec extends FlatSpec with Matchers with PropertyChecks {
  import FreeLDAPAuthenticationService._

  // Silly mock of a user database
  case class MockAuthDB(users: Map[UID, (String, DisplayName)], acceptEmptyPwd: Boolean) {
    def authenticate(u: String, p: String): UID = {
      // This checks if the username and password but lets it bypass it
      if (users.contains(u) && ((u == p && p.nonEmpty) || acceptEmptyPwd)) u else throw new RuntimeException()
    }
    def displayName(uid: UID): DisplayName = ~users.get(uid).map(_._2)
  }

  // Natural transformation to Id with a mock auth db
  def toMockDB(db: MockAuthDB): LdapOp ~> Id =
    new (LdapOp ~> Id) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p) => db.authenticate(u, p)
          case LdapOp.UserDisplayNameOp(uid: UID) => db.displayName(uid)
      }
    }

  def runMock[A](a: LdapM[A], db: MockAuthDB): A =
    a.foldMap(toMockDB(db))

  "LDAP Auth Service" should "support auth" in {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = true)
      runMock(authenticationProgram(u, ""), db) == UserDetails(u, t._2)
    }
  }
  it should "suport auth with a password" in {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = false)
      intercept[RuntimeException] {
        runMock(authenticationProgram(u, t._1), db) == UserDetails(u, t._2)
      }
    }
  }
  it should "handle exceptions" in {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = false)
      intercept[RuntimeException] {
        runMock(authenticationProgram(u, ""), db)
      }
    }
  }
}
