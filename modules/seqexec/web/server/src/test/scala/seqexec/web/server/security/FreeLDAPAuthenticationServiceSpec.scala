// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import seqexec.model.UserDetails
import seqexec.model.UserDetails._
import cats._
import cats.tests.CatsSuite

class FreeLDAPAuthenticationServiceSpec extends CatsSuite {
  import FreeLDAPAuthenticationService._

  // Silly mock of a user database
  case class MockAuthDB(users: Map[UID, (String, DisplayName)], acceptEmptyPwd: Boolean) {
    def authenticate(u: String, p: String): UID =
      // This checks if the username and password but lets it bypass it
      if (users.contains(u) && ((u === p && p.nonEmpty) || acceptEmptyPwd)) u else throw new RuntimeException()

    def displayName(uid: UID): DisplayName = users.get(uid).map(_._2).getOrElse("")
  }

  // Natural transformation to Id with a mock auth db
  def toMockDB(db: MockAuthDB): LdapOp ~> Id =
    new (LdapOp ~> Id) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p)       => db.authenticate(u, p)
          case LdapOp.UserDisplayNameOp(uid)     => db.displayName(uid)
          case LdapOp.DisplayNameGrpThumbOp(_)   => ("", Nil, None)
      }
    }

  def runMock[A](a: LdapM[A], db: MockAuthDB): A =
    a.foldMap(toMockDB(db))

  test("LDAP Auth Service: support simple auth") {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = true)
      runMock(authenticate(u, ""), db) shouldEqual u
    }
  }
  test("LDAP Auth Service: support auth and name") {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = true)
      runMock(authenticationAndName(u, ""), db) shouldEqual UserDetails(u, t._2)
    }
  }
  test("LDAP Auth Service: support auth and name with a password") {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = false)
      intercept[RuntimeException] {
        runMock(authenticationAndName(u, t._1), db) shouldEqual UserDetails(u, t._2)
      }
    }
  }
  test("LDAP Auth Service: handle exceptions") {
    forAll { (u: String, t: (String, String)) =>
      val db = MockAuthDB(Map(u -> t), acceptEmptyPwd = false)
      intercept[RuntimeException] {
        runMock(authenticationAndName(u, ""), db)
      }
    }
  }
}
